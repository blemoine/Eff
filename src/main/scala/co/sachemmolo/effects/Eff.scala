package co.sachemmolo.effects

import cats.Monad
import shapeless.{::, HList, HNil}
import shapeless.ops.hlist.SelectAll


trait EFFECT

sealed trait Eff[E <: HList, A] {
  def map[B](fn: A => B): Eff[E, B]

  def flatMap[C, G <: HList](fn: A => Eff[G, C])(implicit union: SelectableUnion[E, G]): Eff[union.Out, C] = Impure(fn, this)

  def run[M[_]](e: E)(implicit handler:Handlers[E, M]): M[A]
}

case class Pure[A](a: A) extends Eff[HNil, A] {
  override def map[B](fn: (A) => B): Eff[HNil, B] = Pure(fn(a))

  def run[M[_]](e: HNil)(implicit handler:Handlers[HNil, M]): M[A] = handler.pure(a)
}

case class OneImpurity[A, E <: EFFECT](gen: E => A) extends Eff[E :: HNil, A] {
  override def map[B](fn: (A) => B): Eff[E :: HNil, B] = OneImpurity((e) => fn(gen(e)))

  def run[M[_]](e: E :: HNil)(implicit handler:Handlers[E :: HNil, M]): M[A] = handler.pure(gen(e.head))
}

class Impure[A, F <: HList, C, G <: HList, R <: HList](gen: A => Eff[G, C], eff: Eff[F, A])(implicit selectAllF: SelectAll[R, F], selectAllG: SelectAll[R, G]) extends Eff[R, C] {
  override def map[B](fn: (C) => B): Eff[R, B] = new Impure((a: A) => gen(a).map(fn), eff)

  override def run[M[_]](e: R)(implicit handler:Handlers[R, M]): M[C] = {
    val ma: M[A] = eff.run[M](selectAllF(e))(handler.subHandler(selectAllF))
    handler.flatMap(ma)(a => gen(a).run[M](selectAllG(e))(handler.subHandler(selectAllG)))
  }
}
object Impure {
  def apply[A, F <: HList, C, G <: HList](gen: A => Eff[G, C], eff: Eff[F, A])(implicit union: SelectableUnion[F, G]): Eff[union.Out, C] = new Impure(gen, eff)(union.selectL, union.selectM)
}