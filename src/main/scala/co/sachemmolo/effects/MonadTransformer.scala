package co.sachemmolo.effects

import cats.arrow.FunctionK
import cats.data.OptionT
import cats.{Monad, ~>}

abstract class MonadTransformer[K[_] : Monad] {

  def flatMap[M[_] : Monad, A, B](fa: M[K[A]])(f: A => M[K[B]]): M[K[B]]

  def tailRecM[M[_] : Monad, A, B](a: A)(f: (A) => M[K[Either[A, B]]]): M[K[B]]

  def wrapped[M[_] : Monad]: K ~> ({type L[X] = M[K[X]]})#L = new FunctionK[K, ({type L[X] = M[K[X]]})#L] {
    override def apply[A](fa: K[A]): M[K[A]] = Monad[M].pure(fa)
  }

  def innerWrapped[M[_] : Monad]: M ~> ({type L[X] = M[K[X]]})#L = new FunctionK[M, ({type L[X] = M[K[X]]})#L] {
    override def apply[A](fa: M[A]): M[K[A]] = Monad[M].map(fa)(Monad[K].pure)
  }

  def monadInstance[M[_] : Monad]: Monad[({type L[X] = M[K[X]]})#L] = {
    val self = this
    new Monad[({type L[X] = M[K[X]]})#L] {
      type L[X] = M[K[X]]
      val monadM = Monad[M]
      val monadK = Monad[K]

      override def pure[A](x: A): L[A] = {
        monadM.pure(monadK.pure(x))
      }

      override def flatMap[A, B](fa: L[A])(f: (A) => L[B]): L[B] = {
        self.flatMap[M, A, B](fa)(f)
      }

      override def tailRecM[A, B](a: A)(f: (A) => L[Either[A, B]]): L[B] = {
        self.tailRecM[M, A, B](a)(f)
      }
    }
  }
}

object TransformerInstance {

  import cats.implicits._
  implicit object OptionTransformer extends MonadTransformer[Option] {
    override def flatMap[M[_] : Monad, A, B](fa: M[Option[A]])(f: (A) => M[Option[B]]): M[Option[B]] = OptionT(fa).flatMap(a => OptionT(f(a))).value

    override def tailRecM[M[_] : Monad, A, B](a: A)(f: (A) => M[Option[Either[A, B]]]): M[Option[B]] = OptionT.catsDataMonadForOptionT[M].tailRecM(a)((a: A) => OptionT(f(a))).value
  }

}