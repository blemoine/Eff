package co.sachemmolo.effects

import cats.Monad
import co.sachemmolo.effects.Eff.RunOne
import shapeless.ops.hlist.SelectAll
import shapeless.{::, HList, HNil}

import scala.reflect.ClassTag


trait EFFECT {
  type R

  type DefaultMonad[_]

  def resources: R
}

case class Resource[E <: HList](resource: E)

object Resource {
  implicit val ResourceHNil: Resource[HNil] = Resource(HNil)

  implicit def ResourceHCons[E <: EFFECT, F <: HList](implicit e: E, f: Resource[F]): Resource[E :: F] = Resource(e :: f.resource)
}

sealed trait Eff[E <: HList, A] {
  def map[B](fn: A => B): Eff[E, B] = flatMap(a => Eff.Pure(() => fn(a)))(SelectableUnion.hlistUnion[E])

  def flatMap[H <: HList, B](fn: A => Eff[H, B])(implicit selectableUnion: SelectableUnion[H, E]): Eff[selectableUnion.Out, B] = Eff.Impure[E, A, H, B, selectableUnion.Out](fn, this)(selectableUnion.selectM, selectableUnion.selectL)

  def run[M[_] : Monad](e: E)(implicit handlers: Handlers[E, M]): M[A]

  def run[M[_] : Monad](implicit e: Resource[E], handlers: Handlers[E, M]): M[A] = run[M](e.resource)


  def runOne[U <: EFFECT, F <: HList, K[_] : Monad](handler: EffectHandler[U, K])(implicit ev: (U :: F) =:= E, u: U, kTransformer: MonadTransformer[K]): Eff[F, K[A]] = {
    RunOne(this.asInstanceOf[Eff[U :: F, A]], u, handler, kTransformer)
  }

  //def runOne[M[_] : Monad, U <: EFFECT : ClassTag, H <: HList](e: U)(implicit ev: (U :: H) =:= E, handler: EffectHandler[U, M]): Eff[H, M[A]]
}


object Eff {

  trait Generator[E <: EFFECT, A] {

    def apply(e: E): E#DefaultMonad[A]

    def map[B](fn: A => B)(implicit monad: Monad[E#DefaultMonad]): Generator[E, B] = {
      val self = this
      new Generator[E, B] {
        override def apply(e: E): E#DefaultMonad[B] = {
          val value: E#DefaultMonad[A] = self.apply(e)
          monad.map[A, B](value)(fn)
        }
      }
    }
  }

  object Generator {
    def apply[E <: EFFECT, A](fn: E#R => E#DefaultMonad[A]): Generator[E, A] = new Generator[E, A] {
      override def apply(e: E): E#DefaultMonad[A] = fn(e.resources)
    }
  }

  def apply[E <: EFFECT : ClassTag, A](fn: E#R => E#DefaultMonad[A]): Eff[E :: HNil, A] = NearPure(Generator[E, A](fn))

  def apply[E <: EFFECT : ClassTag, A](gen: Generator[E, A]): Eff[E :: HNil, A] = NearPure(gen)


  case class Pure[A](a: () => A) extends Eff[HNil, A] {
    override def run[M[_] : Monad](e: HNil)(implicit handlers: Handlers[HNil, M]): M[A] = Monad[M].pure(a())
  }

  case class NearPure[E <: EFFECT : ClassTag, A](gen: Generator[E, A]) extends Eff[E :: HNil, A] {
    override def run[M[_] : Monad](e: ::[E, HNil])(implicit handlers: Handlers[::[E, HNil], M]): M[A] = {
      val maybeHandler: Option[EffectHandler[E, M]] = handlers.handlersMap.get(implicitly[ClassTag[E]]).map(_.asInstanceOf[EffectHandler[E, M]])
      maybeHandler.getOrElse(throw new Exception(s"Could not happen, the handler for ${e.head} should exist")).pure(gen.apply(e.head))
    }
  }

  case class Impure[F <: HList, A, G <: HList, C, R <: HList](gen: A => Eff[G, C], eff: Eff[F, A])(implicit selectF: SelectAll[R, F], selectG: SelectAll[R, G]) extends Eff[R, C] {
    override def run[M[_] : Monad](e: R)(implicit handlers: Handlers[R, M]): M[C] = {
      implicit val handlersF: Handlers[F, M] = handlers.select(selectF)
      implicit val handlersG: Handlers[G, M] = handlers.select(selectG)
      val head: M[A] = eff.run(selectF(e))
      Monad[M].flatMap(head) { a =>
        gen(a).run(selectG(e))
      }
    }
  }

  case class RunOne[E <: EFFECT, F <: HList, K[_] : Monad, C](eff: Eff[E :: F, C], u: E, handler: EffectHandler[E, K], kTransformer: MonadTransformer[K]) extends Eff[F, K[C]] {
    override def run[M[_] : Monad](e: F)(implicit outerHhandlers: Handlers[F, M]): M[K[C]] = {

      type L[X] = M[K[X]]
      val head = handler.transform[L](kTransformer.wrapped)
      val tail: Handlers[F, L] = outerHhandlers.transform[L](kTransformer.innerWrapped[M])
      implicit val handlers: Handlers[E :: F, L] = Handlers.consHandlers(head, tail)
      implicit val monadL = kTransformer.monadInstance[M]
      eff.run[L](u :: e)
    }
  }

}
