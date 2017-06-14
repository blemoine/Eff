package co.sachemmolo.effects

import cats.{Id, Monad}
import shapeless.{::, HList, HNil}

import scala.reflect.ClassTag


trait EFFECT {
  type R

  type DefaultMonad[_]

  def resources: R
}

case class Resource[E <: HList](resource:E)
object Resource {
  implicit val ResourceHNil:Resource[HNil] = Resource(HNil)
  implicit def ResourceHCons[E <: EFFECT, F <: HList](implicit e:E, f:Resource[F]):Resource[E :: F] = Resource(e :: f.resource)
}

sealed trait Eff[E <: HList, A] {
  def map[B](fn: A => B): Eff[E, B] = flatMap(a => Eff.Pure(() => fn(a)))(SelectableUnion.hlistUnion[E])

  def flatMap[H <: HList, B](fn: A => Eff[H, B])(implicit selectableUnion: SelectableUnion[H, E]): Eff[selectableUnion.Out, B] = Eff.impure(fn, this)

  def run[M[_] : Monad](e: E)(implicit handlers: Handlers[E, M]): M[A]

  def run[M[_] : Monad](implicit e: Resource[E], handlers: Handlers[E, M]): M[A] = run[M](e.resource)
}


object Eff {

  trait Generator[E <: EFFECT, A] {

    def apply[M[_] : Monad](e: E, handle: EffectHandler[E, M]): M[A]

    def map[B](fn: A => B): Generator[E, B] = {
      val self = this
      new Generator[E, B] {
        override def apply[M[_] : Monad](e: E, handle: EffectHandler[E, M]): M[B] = implicitly[Monad[M]].map(self.apply(e, handle))(fn)
      }
    }
  }
  object Generator {
    def apply[E <: EFFECT, A](fn: E#R => E#DefaultMonad[A]): Generator[E, A] = new Generator[E, A] {
      override def apply[M[_] : Monad](e: E, handle: EffectHandler[E, M]): M[A] = handle.pure(fn(e.resources))
    }
  }

  def apply[E <: EFFECT : ClassTag, A](fn: E#R => E#DefaultMonad[A]): Eff[E :: HNil, A] = NearPure(Generator[E, A](fn))

  def apply[E <: EFFECT : ClassTag, A](gen: Generator[E, A]): Eff[E :: HNil, A] = NearPure(gen)


  case class Pure[A](a: () => A) extends Eff[HNil, A] {
    override def run[M[_] : Monad](e: HNil)(implicit handlers: Handlers[HNil, M]): M[A] = implicitly[Monad[M]].pure(a())
  }
  case class NearPure[E <: EFFECT : ClassTag, A](gen: Generator[E, A]) extends Eff[E :: HNil, A] {
    override def run[M[_] : Monad](e: ::[E, HNil])(implicit handlers: Handlers[::[E, HNil], M]): M[A] = {
      val maybeHandler: Option[EffectHandler[E, M]] = handlers.handlersMap.get(implicitly[ClassTag[E]]).map(_.asInstanceOf[EffectHandler[E, M]])
      gen.apply(e.head, maybeHandler.getOrElse(throw new Exception("Could not happen")))
    }
  }

  private[effects] def impure[F <: HList, A, G <: HList, C](gen: A => Eff[G, C], eff: Eff[F, A])(implicit selectableUnion: SelectableUnion[G, F]): Eff[selectableUnion.Out, C] = new Eff[selectableUnion.Out, C] {
    def run[M[_] : Monad](e: selectableUnion.Out)(implicit handlers: Handlers[selectableUnion.Out, M]): M[C] = {
      implicit val handlersF: Handlers[F, M] = handlers.select(selectableUnion.selectM)
      implicit val handlersG: Handlers[G, M] = handlers.select(selectableUnion.selectL)
      val head: M[A] = eff.run(selectableUnion.selectM(e))
      implicitly[Monad[M]].flatMap(head)(a => gen(a).run(selectableUnion.selectL(e)))
    }
  }
}
