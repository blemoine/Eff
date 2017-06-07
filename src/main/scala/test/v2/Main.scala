package co.sachemmolo.v3.effects

import cats.Monad
import co.sachemmolo.effects.Eff.Generator
import co.sachemmolo.effects.{EFFECT, Eff, EffectHandler}
import co.sachemmolo.effects.effects.Console
import co.sachemmolo.effects.effects.Console._
import co.sachemmolo.effects.effects.Rnd._
import co.sachemmolo.effects.effects.TryCatch
import co.sachemmolo.effects.effects.TryCatch._
import shapeless._

import scala.concurrent.{ExecutionContext, Future}



object Main {


  val tenOrA: Eff[RND :: HNil, String] = rnd { i =>
    if (i > 0.5) {
      "10"
    } else {
      "A"
    }
  }



  import cats.implicits._
  def x = tenOrA.run[Option]

  import shapeless._
  import cats.implicits._
  import co.sachemmolo.effects.Eff

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

  sumRnd.run[Option] // return Some("1010") or None
  //Also display in the console (for example)
  //  (s1,10)
  //  (s2,10)
  //  (result,1010)

  /*
  def main(args: Array[String]): Unit = {
    val z: Eff[::[Async.ASYNC, HNil], Int] = for {
      a <- Async.async(e => Future(2)(e))
      b <- Async.async(e => Future(3)(e))
    } yield 2 + 3

    import Async._
    import cats.implicits._

    implicit val ec = DefaultEc.resources
    z.run[Future](DefaultEc :: HNil).onComplete { r => println(r)

    }
  }
  */

}


object Async {
  trait ASYNC extends EFFECT {
    override type R = ExecutionContext
    override type DefaultMonad[X] = Future[X]
  }
  implicit object DefaultEc extends ASYNC {
    override def resources: ExecutionContext = ExecutionContext.global
  }

  def async[A](fn: ExecutionContext => Future[A]):Eff[ASYNC :: HNil, A] = Eff[ASYNC, A](fn)

  implicit def handler: EffectHandler[ASYNC, Future] = new EffectHandler[ASYNC, Future] {
    override def pure[A](a: => Future[A]): Future[A] = a
  }
}