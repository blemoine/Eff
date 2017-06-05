package co.sachemmolo.effects

import cats.Monad
import shapeless.{::, HList, HNil}


trait EFFECT {
  type R

  def resources: R
}


sealed trait Eff[E <: HList, A] {
  def map[B](fn: A => B): Eff[E, B]

  def flatMap[H <: HList, B](fn: A => Eff[H, B])(implicit selectableUnion: SelectableUnion[E, H]): Eff[selectableUnion.Out, B] = Eff.impure(fn, this)

  def run[M[_] : Monad](e: E)(implicit handlers: Handlers[E, M]): M[A]
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
    def apply[E <: EFFECT, A](fn: E#R => A): Generator[E, A] = new Generator[E, A] {
      override def apply[M[_] : Monad](e: E, handle: EffectHandler[E, M]): M[A] = handle.pure(fn(e.resources))
    }
  }

  def apply[E <: EFFECT, A](fn: E#R => A): Eff[E :: HNil, A] = nearPure(Generator[E, A](fn))

  def apply[E <: EFFECT, A](gen: Generator[E, A]): Eff[E :: HNil, A] = nearPure(gen)

  private[effects] def nearPure[E <: EFFECT, A](gen: Generator[E, A]): Eff[E :: HNil, A] = new Eff[E :: HNil, A] {
    override def map[B](fn: (A) => B): Eff[E :: HNil, B] = nearPure(gen.map(fn))

    override def run[M[_] : Monad](e: E :: HNil)(implicit handlers: Handlers[E :: HNil, M]): M[A] = {
      val maybeHandler: Option[EffectHandler[E, M]] = handlers.handlersMap.get(e.head).map(_.asInstanceOf[EffectHandler[E, M]])
      gen.apply(e.head, maybeHandler.getOrElse(throw new Exception("Could not happen")))
    }
  }

  private[effects] def impure[F <: HList, A, G <: HList, C](gen: A => Eff[G, C], eff: Eff[F, A])(implicit selectableUnion: SelectableUnion[F, G]): Eff[selectableUnion.Out, C] = new Eff[selectableUnion.Out, C] {
    override def map[B](fn: (C) => B): Eff[selectableUnion.Out, B] = impure((a: A) => gen(a).map(fn), eff)

    def run[M[_] : Monad](e: selectableUnion.Out)(implicit handlers: Handlers[selectableUnion.Out, M]): M[C] = {
      implicit val handlersF: Handlers[F, M] = handlers.select(selectableUnion.selectL)
      implicit val handlersG: Handlers[G, M] = handlers.select(selectableUnion.selectM)
      val head: M[A] = eff.run(selectableUnion.selectL(e))
      implicitly[Monad[M]].flatMap(head)(a => gen(a).run(selectableUnion.selectM(e)))
    }
  }
}
