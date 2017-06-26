package test.v2

import cats.implicits._
import co.sachemmolo.effects.Eff
import co.sachemmolo.effects.effects.Console._
import co.sachemmolo.effects.effects.Rnd._
import co.sachemmolo.effects.effects.TryCatch
import co.sachemmolo.effects.effects.TryCatch._
import shapeless._

object Main {
  def main(args: Array[String]): Unit = {

    def toGuess: Eff[RND :: HNil, Int] = rnd(d => Math.floor(d * 10).toInt)

    val eff: Eff[CONSOLE :: EXCEPTION :: RND :: HNil, Unit] = for {
      nbToGuess <- toGuess
      r <- guess(nbToGuess)
    } yield r

    eff.run[Option]
  }


  def guess(toGuess: Int): Eff[CONSOLE :: EXCEPTION :: HNil, Unit] = {
    for {
      _ <- withConsole(c => c.out.println("guess a number?"))
      nbStr <-  withConsole(c => c.in.readLine())
      guessed <- TryCatch(nbStr.toInt)
      r <- if (guessed == toGuess) {
        TryCatch(()).flatMap(_ => withConsole(c => c.out.println(s"You correctly guessed $toGuess")))
      } else {
        (if(guessed > toGuess) {
          withConsole(c => c.out.println(s"The number to guess is lesser than $guessed"))
        } else {
          withConsole(c => c.out.println(s"The number to guess is greater than $guessed"))
        }).flatMap(_ => guess(toGuess))
      }
    } yield r
  }

}
