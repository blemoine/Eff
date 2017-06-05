package co.sachemmolo.effects

import shapeless._
import shapeless.ops.hlist.{FilterNot, Remove, SelectAll}

trait SelectableUnion[L <: HList, M <: HList] extends DepFn2[L, M] with Serializable {
    type Out <: HList

    def selectL: SelectAll[Out, L]

    def selectM: SelectAll[Out, M]
  }

  trait LowPriorityUnion {
    type Aux[L <: HList, M <: HList, Out0 <: HList] = SelectableUnion[L, M] {type Out = Out0}
  }

  object SelectableUnion extends LowPriorityUnion {

    implicit def lubToSpecificConstraint[H, L <: HList, B](implicit lub:LUBConstraint[H :: L, B]): H <:< B = ???

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

//        override def constraint[TC](implicit tcl: LUBConstraint[HNil, TC], tcm: LUBConstraint[M, TC]): LUBConstraint[M, TC] = tcm
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
