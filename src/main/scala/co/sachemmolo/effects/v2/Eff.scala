package co.sachemmolo.effects.v2

import cats._
import cats.implicits._
import co.sachemmolo.effects.v2.Handler.Handlers
import co.sachemmolo.effects.v2.SelectShapeless.SelectableUnion
import shapeless._
import shapeless.ops.hlist.{FilterNot, Remove, SelectAll, Selector, Union}

trait EFFECT

trait Eff[E <: HList, A] {

  def run(e:E):A

  def map[B](fn: A => B): Eff[E, B] = {
    val self = this
    new Eff[E, B] {
      override def run(e:E): B = fn(self.run(e))
    }
  }

  def flatMap[B, F <: HList](fn: A => Eff[F, B])(implicit union: SelectableUnion[E, F]): Eff[union.Out, B] = {
    val self = this
    import SelectShapeless._
    val selectAllE:SelectAll[union.Out, E] = union.selectL
    val selectAllF:SelectAll[union.Out, F] = union.selectM
    new Eff[union.Out, B] {
      override def run(u: union.Out): B = fn(self.run(selectAllE(u))).run(selectAllF(u))
    }
  }



}

object SelectShapeless {
  trait SelectableUnion[L <: HList, M <: HList] extends DepFn2[L, M] with Serializable {
    type Out <: HList

    def selectL:SelectAll[Out, L]
    def selectM:SelectAll[Out, M]
  }

  trait LowPriorityUnion {
    type Aux[L <: HList, M <: HList, Out0 <: HList] = SelectableUnion[L, M] { type Out = Out0 }
  }

  object SelectableUnion extends LowPriorityUnion {
    def apply[L <: HList, M <: HList](implicit union: SelectableUnion[L, M]): Aux[L, M, union.Out] = union

    // let ∅ ∪ M = M
    implicit def hlistUnion[M <: HList]: Aux[HNil, M, M] =
      new SelectableUnion[HNil, M] {
        type Out = M
        def apply(l: HNil, m: M): Out = m

        override def selectL: SelectAll[M, HNil] = SelectAll.hnilSelectAll

        override def selectM: SelectAll[M, M] = new SelectAll[M, M] {
          override def apply(t: M): M = t
        }
      }

    // let (H :: T) ∪ M  =  H :: (T ∪ M) when H ∉ M
    implicit def hlistUnion1[H, T <: HList, M <: HList]
    (implicit
      u: SelectableUnion[T, M],
      f: FilterNot.Aux[M, H, M]
    ): Aux[H :: T, M, H :: u.Out] =
      new SelectableUnion[H :: T, M] {
        type Out = H :: u.Out
        def apply(l: H :: T, m: M): Out = l.head :: u(l.tail, m)

        override def selectL: SelectAll[::[H, u.Out], ::[H, T]] = new SelectAll[H :: u.Out, H :: T] {
          override def apply(t: H :: u.Out): H :: T = (t.head) :: u.selectL(t.tail)
        }

        override def selectM: SelectAll[::[H, u.Out], M] = new SelectAll[H :: u.Out, M] {
          override def apply(t: ::[H, u.Out]): M = u.selectM.apply(t.tail)
        }
      }

    // let (H :: T) ∪ M  =  H :: (T ∪ (M - H)) when H ∈ M
    implicit def hlistUnion2[H, T <: HList, M <: HList, MR <: HList]
    (implicit
      r: Remove.Aux[M, H, (H, MR)],
      u: SelectableUnion[T, MR]
    ): Aux[H :: T, M, H :: u.Out] =
      new SelectableUnion[H :: T, M] {
        type Out = H :: u.Out
        def apply(l: H :: T, m: M): Out = l.head :: u(l.tail, r(m)._2)

        override def selectL: SelectAll[::[H, u.Out], ::[H, T]] = new SelectAll[H :: u.Out, H :: T] {
          override def apply(t: H :: u.Out): H :: T = (t.head) :: u.selectL(t.tail)
        }

        override def selectM: SelectAll[::[H, u.Out], M] = new SelectAll[H :: u.Out, M] {
          override def apply(t: ::[H, u.Out]): M = r.reinsert(t.head, u.selectM(t.tail))
        }
      }
  }

}


object Handler {

  abstract class EffectHandler[E <: EFFECT, M[_] : Monad] {
    def handle[A, F <: HList](eff: Eff[E :: F, A]): Eff[F, M[A]]
  }

  trait Handlers[F <: HList, M[_]] {
    def handler[A](eff: Eff[F, M[A]]): Eff[HNil, M[A]]
  }

  implicit def nilHandlers[M[_] : Monad]: Handlers[HNil, M] = new Handlers[HNil, M] {
    override def handler[A](eff: Eff[HNil, M[A]]): Eff[HNil, M[A]] = eff
  }

  implicit def handlers[E <: EFFECT, F <: HList, M[_] : Monad](implicit headHandler: EffectHandler[E, M], tailHandler: Handlers[F, M]): Handlers[E :: F, M] = new Handlers[E :: F, M] {
    override def handler[A](eff: Eff[E :: F, M[A]]): Eff[HNil, M[A]] = {
      val r: Eff[F, M[A]] = headHandler.handle(eff).map(mma => implicitly[Monad[M]].flatten(mma))
      tailHandler.handler(r)
    }
  }

  def handlePure[A](eff: Eff[HNil, A]): A = eff.run(HNil)

  def handle[A, F <: HList, M[_] : Monad](eff: Eff[F, A])(implicit handlers: Handlers[F, M]): M[A] = {
    val r = handlers.handler(eff.map(a => implicitly[Monad[M]].pure(a)))

    handlePure(r)
  }

  class Handler[M[_] : Monad] {
    def handle[A, F <: HList](eff: Eff[F, A])(implicit handlers: Handlers[F, M]): M[A] = Handler.handle(eff)(implicitly[Monad[M]], handlers)
  }

  def apply[M[_] : Monad]: Handler[M] = new Handler[M]
}
