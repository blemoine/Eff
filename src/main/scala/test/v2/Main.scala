package co.sachemmolo.v3.effects

import co.sachemmolo.effects.Eff
import co.sachemmolo.effects.effects.Console
import co.sachemmolo.effects.effects.Console._
import co.sachemmolo.effects.effects.Rnd._
import co.sachemmolo.effects.effects.TryCatch
import co.sachemmolo.effects.effects.TryCatch._
import shapeless._



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

}