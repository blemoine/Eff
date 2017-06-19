package co.sachemmolo.effects

import cats.Monad
import co.sachemmolo.effects.effects.Rnd.RND
import co.sachemmolo.effects.effects.TryCatch.EXCEPTION
import co.sachemmolo.effects.effects.{Rnd, TryCatch}
import org.scalatest.{Matchers, WordSpec}
import shapeless._
import cats.implicits._

import scala.util.{Failure, Success, Try}

class ExampleSpec extends WordSpec with Matchers {

  case class TooBigException(n: Double) extends RuntimeException

  "base case of original paper" should {

    "work with inserting" in {
      val ex2: Eff[EXCEPTION :: RND :: HNil, Double] = for {
        x <- Rnd.rnd(_ * 2)
        y <- TryCatch(if (x < 5) x else throw TooBigException(x))
      } yield y

      implicit val d = new RND {
        override def resources: Double = 1.5
      }
      ex2.run[Option] shouldBe Some(3.0)
    }

    "don't work in the number is bigger than 5" in {
      val ex2: Eff[EXCEPTION :: RND :: HNil, Double] = for {
        x <- Rnd.rnd(_ * 2)
        y <- TryCatch(if (x < 5) x else throw TooBigException(x))
      } yield y

      implicit val d = new RND {
        override def resources: Double = 3.0
      }
      ex2.run[Option] shouldBe None
    }


    "catch the insertion" in {
      val ex2: Eff[EXCEPTION :: RND :: HNil, Double] = for {
        x <- Rnd.rnd(_ * 2)
        y <- TryCatch(if (x < 5) x else throw TooBigException(x))
      } yield y

      val recoveredEx2 = for {
        x <- ex2.runOne(TryCatch.tryHandler)
        u <- TryCatch.apply(x.recover {
          case TooBigException(v) if v < 7 => v
        }.get)
      } yield u

      implicit val d = new RND {
        override def resources: Double = 3.0
      }
      ex2.run[Option] shouldBe None
      recoveredEx2.run[Option] shouldBe Some(6.0)
    }
  }

  private implicit val tryTransformer: MonadTransformer[Try] = new MonadTransformer[Try] {
    override def flatMap[F[_] : Monad, A, B](fa: F[Try[A]])(f: (A) => F[Try[B]]): F[Try[B]] = {
      val F = implicitly[Monad[F]]

      F.flatMap(fa) {
        case Success(a) => f(a)
        case Failure(exception) => F.pure(Failure(exception))
      }
    }

    override def tailRecM[F[_] : Monad, A, B](a: A)(f: (A) => F[Try[Either[A, B]]]): F[Try[B]] = {
      val F = implicitly[Monad[F]]
      F.tailRecM(a) { a0 =>
        F.map(f(a0)) {
          case Success(eitherAB) => eitherAB.map(b => Success(b))
          case Failure(exception) => Either.right(Failure(exception))
        }
      }
    }
  }
}