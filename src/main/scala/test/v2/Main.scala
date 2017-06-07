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
  def x = tenOrA.run[Option](DefaultRnd :: HNil)



  def parse(str: String): Eff[EXCEPTION :: HNil, Int] = {
    TryCatch[Int](str.toInt)
  }



  val sumRnd: Eff[RND :: CONSOLE :: EXCEPTION :: HNil, Int] = for {
    s1 <- tenOrA
    _ <- Console.withConsole(console => console.println("s1", s1))
    s2 <- tenOrA
    _ <- Console.withConsole(console => console.println("s2", s2))
    result <- parse(s1 + s2)
    _ <- Console.withConsole(console => console.println("result", result))
  } yield result

  def y  = sumRnd.run[Option](DefaultRnd :: Console.DefaultConsole :: EXCEPTION :: HNil)

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

}


object Async {
  trait ASYNC extends EFFECT {
    override type R = ExecutionContext
    override type DefaultMonad[X] = Future[X]
  }
  implicit object DefaultEc extends ASYNC {
    override def resources: ExecutionContext = ExecutionContext.global
  }

  def async[A](fn: ExecutionContext => Future[A]):Eff[ASYNC :: HNil, A] = Eff(new Generator[ASYNC, A] {
    override def apply[M[_] : Monad](e: ASYNC, handle: EffectHandler[ASYNC, M]): M[A] = handle.pure(fn(e.resources))
  })

  implicit def handler: EffectHandler[ASYNC, Future] = new EffectHandler[ASYNC, Future] {
    override def pure[A](a: => Future[A]): Future[A] = a
  }
}