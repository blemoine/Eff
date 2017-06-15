package test.v2

import cats.Monad
import co.sachemmolo.effects.effects.Rnd._
import co.sachemmolo.effects.effects.TryCatch
import co.sachemmolo.effects.effects.TryCatch._
import cats.implicits._
import co.sachemmolo.effects.MonadTransformer
import co.sachemmolo.effects._
import shapeless._

import scala.util.{Failure, Success, Try}

object Main {

  case class TooBigException(nb: Int) extends RuntimeException {

  }


  def main(args: Array[String]): Unit = {


    val z: Eff[EXCEPTION :: RND :: HNil, Int] = for {
      v <- rnd[Int](i => 6)//Math.abs(i * 10).toInt)
      r <- TryCatch.apply {
        if (v > 5) throw TooBigException(v) else v
      }
    } yield r

    implicit val tryTransformer: MonadTransformer[Try] = new MonadTransformer[Try] {
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
    val z2: Eff[RND :: HNil, Try[Int]] = z.runOne(TryCatch.tryHandler)

    val z3: Eff[EXCEPTION :: RND :: HNil, Int] = for {
      x <- z2
      u <- TryCatch.apply(x.recover {
        case TooBigException(v) if v < 7 => v
      }.get)
    } yield u


    println("z", z.run[Option])
    println("z2", z2.run[Option])
    println("z3", z3.run[Option])
  }

}