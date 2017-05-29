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

```tut:silent
import co.sachemmolo.effects.Eff
import co.sachemmolo.effects.Handler
import co.sachemmolo.effects.effects.Rnd._
import shapeless._
import cats.implicits._

val tenOrA: Eff[RND :: HNil, String] = rnd { i => 
  if (i > 0.5) {
    "10"
  } else {
    "A"
  }
}

Handler[Option].handle(tenOrA) // Some("10") or Some("A")
```

More Advanced example
---------------------

```tut:silent
import shapeless._
import cats.implicits._
import co.sachemmolo.effects.Eff
import co.sachemmolo.effects.Handler
import co.sachemmolo.effects.effects.Rnd._
import co.sachemmolo.effects.effects.Console._
import co.sachemmolo.effects.effects.TryCatch._

//Parsing a String to Int can throw an Exception
// that can be represented as an Effect
def parse(str: String): Eff[EXCEPTION :: HNil, Int] = {
      TryCatch[Int](str.toInt)
}

// sumRnd will use something random (RND) , can throw an exception (EXCEPTION)
 // and will write or read from the console (CONSOLE)
val sumRnd: Eff[RND :: CONSOLE :: EXCEPTION :: HNil, Int] = for {
    s1 <- tenOrA
    _ <- withConsole(console => console.println("s1", s1))
    s2 <- tenOrA
    _ <- withConsole(console => console.println("s2", s2))
    result <- parse(s1 + s2)
    _ <- withConsole(console => console.println("result", result))
} yield result

//For the moment nothing is printed in the console.
// The code will be executed only when handled

Handler[Option].handle(sumRnd) // return Some("1010") or None
//Also display in the console (for example) 
//  (s1,10)
//  (s2,10)
//  (result,1010)
```

Declaring a new EFFECT
----------------------

As an example, will we write here a new EFFECT that will manipulate the current time

```tut:silent
import java.time.Instant
import co.sachemmolo.effects.EFFECT
import co.sachemmolo.effects.Eff
import shapeless._
import cats.implicits._

  //We wrap in an Object our Effect to prevent leaking of implicits  
  object Time {

    //First we want to declare the TIME EFFECT and it's associated resource
    trait TIME extends EFFECT {
      def resource: Instant
    }

    // Give this EFFECT a default implementation, for example now():
    implicit object DEFAULT_TIME extends TIME {
      override def resource: Instant = Instant.now()
    }

    //Then we can declare the constructor or our Effectful computation:

    // The EFFECT list is an HList, here we declare only one EFFECT, but it's not mandatory


    def time[A](fn: Instant => A): Eff[TIME :: HNil, A] = Eff(e => fn(e.head.resource))

    //Finally we have to declare our Handler
    //Here we write something like "if we have TIME effect, we can get the value in an Option"
    // But in fact, this cannot fail, so here the EFFECT TIME is only present as an information
    // of which impure resource the function is using
    import co.sachemmolo.effects.EffectHandler

    implicit def optionHandler(implicit time: TIME): EffectHandler[TIME, Option] = new EffectHandler[TIME, Option] {
      override def handle[A, F <: HList](eff: Eff[TIME :: F, A]): Eff[F, Option[A]] = Eff(e => Some(eff.run(time :: e)))
      override def handlePure[A](eff: Eff[::[TIME, HNil], A]): Option[A] = Some(eff.run(time :: HNil))
    }
  }

  //We can now use our new effect
  import java.time.ZoneOffset
  import co.sachemmolo.effects.Handler
  import Time.TIME
  import Time.time

  val nowAsString: Eff[TIME :: HNil, String] = time(_.atOffset(ZoneOffset.UTC).toString)

  val now1 = Handler[Option].handle(nowAsString) // return the current DateTime as a String

  //but we can also use another resource for this effect
  object WithEpochTime {

    implicit object FIXED_TIME extends TIME {
      override def resource: Instant = Instant.ofEpochMilli(0)
    }

    val now2 = Handler[Option].handle(nowAsString) // return 1st of january 1970 as a String
  }
```
 
