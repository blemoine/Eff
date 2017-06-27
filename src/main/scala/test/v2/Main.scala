package test.v2

import cats.implicits._
import co.sachemmolo.effects.Eff
import co.sachemmolo.effects.effects.Input._
import co.sachemmolo.effects.effects.Output._
import co.sachemmolo.effects.effects.Rnd._
import co.sachemmolo.effects.effects.TryCatch
import co.sachemmolo.effects.effects.TryCatch._
import shapeless._

object Main {
  def main(args: Array[String]): Unit = {

    val eff: Eff[INPUT :: EXCEPTION :: OUTPUT :: RND :: HNil, Unit] = guessGame()

    eff.run[Option]
  }

  def guessGame():Eff[INPUT :: EXCEPTION :: OUTPUT :: RND :: HNil, Unit] = for {
    nbToGuess <- rndInt(0, 10)
    r <- guess(nbToGuess)
  } yield r

  def guess(toGuess: Int): Eff[INPUT :: EXCEPTION :: OUTPUT :: HNil, Unit] = {
    for {
      _ <- printlnEff("guess a number?")
      nbStr <- readLine()
      guessed <- TryCatch(nbStr.toInt)
      r <- if (guessed == toGuess) {
        printlnEff(s"You correctly guessed $toGuess")
          .flatMap(_ => Eff.pure[INPUT :: EXCEPTION :: OUTPUT :: HNil, Unit](()))
      } else {
        (if (guessed > toGuess) {
          printlnEff(s"The number to guess is lesser than $guessed")
        } else {
          printlnEff(s"The number to guess is greater than $guessed")
        }).flatMap(_ => guess(toGuess))
      }
    } yield r
  }

}
