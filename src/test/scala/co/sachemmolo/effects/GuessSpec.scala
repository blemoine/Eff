package co.sachemmolo.effects

import java.io.{BufferedReader, ByteArrayOutputStream, PrintStream, StringReader}

import org.scalatest.{Matchers, WordSpec}
import co.sachemmolo.effects.effects.Input.INPUT
import co.sachemmolo.effects.effects.Output.OUTPUT
import co.sachemmolo.effects.effects.Rnd.RND
import co.sachemmolo.effects.effects.TryCatch.EXCEPTION
import cats.implicits._


class GuessSpec extends WordSpec with Matchers {

  "readNumber" should {
    "read a number from the input" in {
      val r = Game.readNumber()
      implicit val inputStub: INPUT = new INPUT {
        override val resources: BufferedReader = new BufferedReader(new StringReader("5"))
      }
      r.run[Option] shouldBe Some(5)
    }

    "return None if the number from input is not parsable" in {
      val r = Game.readNumber()
      implicit val inputStub: INPUT = new INPUT {
        override val resources: BufferedReader = new BufferedReader(new StringReader("invalid"))
      }
      r.run[Option] shouldBe None
    }
  }

  "guess" should {
    "exit if the number is guessed" in {
      val r = Game.guess(5)
      implicit val inputStub: INPUT = new INPUT {
        override def resources: BufferedReader = new BufferedReader(new StringReader("5"))
      }
      val bo = new ByteArrayOutputStream()
      implicit val outputStub: OUTPUT = new OUTPUT {
        override def resources: PrintStream = new PrintStream(bo)
      }

      r.run[Option] shouldBe Some(())
      bo.flush()

      val allWrittenLines = new String(bo.toByteArray)
      val lines = allWrittenLines.split("\n")
      lines.length shouldBe 2 //The first line ask you to guess
      lines(1) should include("Success!")
    }
    "loop with a message indicating greater than is the number is greater" in {
      val r = Game.guess(3)
      implicit val inputStub: INPUT = new INPUT {
        override val resources: BufferedReader = new BufferedReader(new StringReader("2\n3"))
      }
      val bo = new ByteArrayOutputStream()
      implicit val outputStub: OUTPUT = new OUTPUT {
        override val resources: PrintStream = new PrintStream(bo)
      }

      r.run[Option] shouldBe Some(())
      bo.flush()

      val allWrittenLines = new String(bo.toByteArray)
      val lines = allWrittenLines.split("\n")
      lines.length shouldBe 4 //The first  and third line ask you to guess
      lines(1) should include("greater")
      lines(3) should include("Success!")
    }
    "loop with a message indicating lesser than is the number is lesser" in {
      val r = Game.guess(3)
      implicit val inputStub: INPUT = new INPUT {
        override val resources: BufferedReader = new BufferedReader(new StringReader("5\n3"))
      }
      val bo = new ByteArrayOutputStream()
      implicit val outputStub: OUTPUT = new OUTPUT {
        override val resources: PrintStream = new PrintStream(bo)
      }

      r.run[Option] shouldBe Some(())
      bo.flush()

      val allWrittenLines = new String(bo.toByteArray)
      val lines = allWrittenLines.split("\n")
      lines.length shouldBe 4 //The first  and third line ask you to guess
      lines(1) should include("lesser")
      lines(3) should include("Success!")
    }
  }

  "start" should {
    "generate a random number to be guessed" in {
      val r = Game.start()
      implicit val inputStub: INPUT = new INPUT {
        override val resources: BufferedReader = new BufferedReader(new StringReader("5\n3"))
      }
      val bo = new ByteArrayOutputStream()
      implicit val outputStub: OUTPUT = new OUTPUT {
        override val resources: PrintStream = new PrintStream(bo)
      }

      implicit val rndStub: RND = new RND {
        override val resources: Double = 0.3
      }

      r.run[Option] shouldBe Some(())
      bo.flush()

      val allWrittenLines = new String(bo.toByteArray)
      val lines = allWrittenLines.split("\n")
      lines.length shouldBe 4 //The first  and third line ask you to guess
      lines(1) should include("lesser")
      lines(3) should include("Success!")
    }
  }


  object Game {

    import co.sachemmolo.effects.effects.Input.{INPUT, readLine}
    import co.sachemmolo.effects.effects.Output.{OUTPUT, printlnEff}
    import co.sachemmolo.effects.effects.Rnd.{RND, rndInt}
    import co.sachemmolo.effects.effects.TryCatch
    import co.sachemmolo.effects.effects.TryCatch.EXCEPTION
    import shapeless.{::, HNil}

    def start(): Eff[INPUT :: EXCEPTION :: OUTPUT :: RND :: HNil, Unit] = for {
      nbToGuess <- rndInt(0, 10)
      r <- guess(nbToGuess)
    } yield r



    def guess(toGuess: Int): Eff[INPUT :: EXCEPTION :: OUTPUT :: HNil, Unit] = {
      for {
        _ <- printlnEff("guess a number?")
        guessed <- readNumber()
        r <- if (guessed == toGuess) {
          printlnEff(s"Success! You correctly guessed $toGuess")
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

    def readNumber(): Eff[EXCEPTION :: INPUT :: HNil, Int] = for {
      nbStr <- readLine()
      guessed <- TryCatch(nbStr.toInt)
    } yield guessed
  }

}