package co.sachemmolo.effects.v2

import cats.Monad
import co.sachemmolo.effects.v2.Console._
import co.sachemmolo.effects.v2.Rnd._
import co.sachemmolo.effects.v2.TryCatch._
import shapeless._
import cats.implicits._
import co.sachemmolo.effects.v2
import co.sachemmolo.effects.v2.Handler.{EffectHandler, Handlers}

object Main {

  val tenOrA = Rnd.rnd(i => if (i > 0.5) {
    "10"
  } else {
    "A"
  })

  val v1: Eff[RND :: HNil, String] = tenOrA.flatMap(s1 => tenOrA.map(s2 => s1 + " " + s2))


    val v2: Eff[RND :: HNil, List[String]] = for {
      s1 <- tenOrA
      s2 <- tenOrA
      s3 <- tenOrA
    } yield List(s1, s2, s3)

    def parse(str: String): Eff[EXCEPTION :: HNil, Int] = {
      TryCatch.TryCatch[Int](str.toInt)
    }

    val v3 = tenOrA.flatMap(parse)

    val v4: Eff[::[RND, ::[CONSOLE, ::[EXCEPTION, HNil]]], Int] = for {
      s1 <- tenOrA
      _ <- withConsole(console => console.println("s1", s1))
      s2 <- tenOrA
      _ <- withConsole(console => console.println("s2", s2))
      result <- parse(s1 + s2)
      _ <- withConsole(console => console.println("result", result))
    } yield result

    def maybeInt = Handler[Option].handle(v4)
}