An Effect System for Scala
==========================

**Don't use this in production! It's only a proof of concept.**
 
Description
-----------

Effect System is a way to deal with _impure_ code in some kind of _pure_ functional programming way.

The core of the Effect System is the `Eff[E, A]` class, which represents an _impure_ code 
that generate a `A` by using a list `E` of `EFFECT`.

An `EFFECT` is a type that represents what kind of resources a computation needs. For example, 
`RND` is an `EFFECT` indicating that something will be random. 
`CONSOLE` is an `EFFECT` indicating that we're reading or writing to the console.
 
Because the point of the Effect System is to manage _impure_ computation that will not always
 give the same result, we cannot simply get a `A` from an `Eff[E, A]`, 
 we necessarily need to get it wrapped in something (an `IO`  monad for example, or simply an `Option`)
 
  
Simple example
--------------

```scala
import co.sachemmolo.effects.Eff
import co.sachemmolo.effects.effects.Rnd.{RND, rnd}
import shapeless._
import cats.implicits._

val tenOrA: Eff[RND :: HNil, String] = rnd { i =>
  if (i > 0.5) {
    "10"
  } else {
    "A"
  }
}

{
  import co.sachemmolo.effects.effects.Rnd._
  tenOrA.run[Option] // Some("10") or Some("A")
}
```

More Advanced example
---------------------


```scala
  import shapeless._
  import cats.implicits._
  import co.sachemmolo.effects.Eff

  import co.sachemmolo.effects.effects.Rnd.{RND, rnd}
  import co.sachemmolo.effects.effects.Console._
  import co.sachemmolo.effects.effects.TryCatch
  import co.sachemmolo.effects.effects.TryCatch._

  //Parsing a String to Int can throw an Exception
  // that can be represented as an Effect
  def parse(str: String): Eff[EXCEPTION :: HNil, Int] = {
    TryCatch[Int](str.toInt)
  }

  // sumRnd will use something random (RND) , can throw an exception (EXCEPTION)
  // and will write or read from the console (CONSOLE)
  val sumRnd: Eff[CONSOLE :: EXCEPTION :: RND :: HNil, Int] = for {
    s1 <- tenOrA
    _ <- withConsole(console => console.println("s1", s1))
    s2 <- tenOrA
    _ <- withConsole(console => console.println("s2", s2))
    result <- parse(s1 + s2)
    _ <- withConsole(console => console.println("result", result))
  } yield result

  //For the moment nothing is printed in the console.
  // The code will be executed only when handled
  {
  import co.sachemmolo.effects.effects.Rnd._
  sumRnd.run[Option] // return Some("1010") or None
  //Also display in the console (for example)
  //  (s1,10)
  //  (s2,10)
  //  (result,1010)
  }
```

Example:
```scala
sumRnd.run[Option]
```

Specifying explicitly a resource for an effect
----------------------------------------------

Effect System should let you pass a specified resource, and not only the default one.
This serves as some kind of dependency injection, and let you test effectful code more easily.

```scala
object MockedRnd {
  import co.sachemmolo.effects.effects.Rnd.{RND, rnd}
  val tenOrA: Eff[RND :: HNil, String] = rnd { i =>
    if (i > 0.9) {
      i.toString
    } else {
      "A"
    }
  }

  import cats.implicits._
  implicit object MockRnd extends RND {
    override def resources: Double = 1.0
  }
  def x = tenOrA.run[Option] // Will always return Some("1.0")
}
```

Example:
```scala
MockedRnd.x
```


Declaring a new EFFECT
----------------------

As an advanced example, will we write here a new EFFECT that will compute asynchronous result

```scala
import scala.concurrent.{Future, ExecutionContext}
import co.sachemmolo.effects.{EFFECT, EffectHandler}

object Async {
   
  //Declaring ASYNC as an Effect
  // Async code is running on various ExecutionContext
  trait ASYNC extends EFFECT {
    override type R = ExecutionContext
    // Async code is by default writtent in a Future
    override type DefaultFunctor[X] = Future[X] 
  }
  //By default, the resource for ASYNC is the global ExecutionContext
  implicit object DefaultAsync extends ASYNC {
    override def resources: ExecutionContext = ExecutionContext.global
  }

  // Constructor for the Asynchronous Effect
  def async[A](fn: ExecutionContext => Future[A]):Eff[ASYNC :: HNil, A] = Eff[ASYNC, A](fn)

  //There is only one Handler for Async code, and it returns a Future
  implicit def handler: EffectHandler[ASYNC, Future] = new EffectHandler[ASYNC, Future] {
    override def pure[A](a: => Future[A]): Future[A] = a
  }
}
```

Now we can use it:

```scala
val z: Eff[Async.ASYNC :: HNil, Int] = for {
  a <- Async.async(e => Future(2)(e))
  b <- Async.async(e => Future(3)(e))
} yield 2 + 3

import Async._
import cats.implicits._

//Future is only a cats Monad IF there is an implicit ExecutionContext in scope
implicit val ec = DefaultAsync.resources
z.run[Future](DefaultAsync :: HNil).onComplete { r => 
  println(r)
}
```
 
