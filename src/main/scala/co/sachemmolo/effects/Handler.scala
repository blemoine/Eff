package co.sachemmolo.effects

import cats.Monad
import shapeless.HList
import shapeless.ops.hlist.SelectAll

trait Handlers[E <: HList, M[_]] {
  def pure[A](a: A): M[A]

  def flatMap[A, B](ma: M[A])(fn: A => M[B]): M[B]

  def subHandler[F <: HList](selectAll: SelectAll[E, F]): Handlers[F, M]
}