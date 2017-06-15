package co.sachemmolo.effects

import cats.arrow.FunctionK
import cats.{Id, Monad, ~>}
import co.sachemmolo.effects.Handlers.HandlersMapEffect
import shapeless.{::, HList, HMap, HNil}
import shapeless.ops.hlist.SelectAll

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import scala.util.Try

abstract class EffectHandler[E <: EFFECT : ClassTag, M[_]] {
  def effect: ClassTag[E] = implicitly[ClassTag[E]]

  def pure[A](a: => E#DefaultMonad[A]): M[A]

  def transform[K[_]](fn: M ~> K): EffectHandler[E, K] = {
    val self = this
    new EffectHandler[E, K] {
      override def pure[A](a: => E#DefaultMonad[A]): K[A] = fn(self.pure(a))
    }
  }
}

object EffectHandler {
  implicit def IdToOption: Id ~> Option = new FunctionK[Id, Option] {
    override def apply[A](fa: Id[A]): Option[A] = Some(fa)
  }
  implicit def TryToOption: Try ~> Option = new FunctionK[Try, Option] {
    override def apply[A](fa: Try[A]): Option[A] = fa.toOption
  }

  def fromMonad[E <: EFFECT : ClassTag, M[_] : Monad](implicit naturalTransfo: E#DefaultMonad ~> M): EffectHandler[E, M] = new EffectHandler[E, M] {
    override def pure[A](a: => E#DefaultMonad[A]): M[A] = {
      val m = Monad[M]
      m.flatten(m.pure(naturalTransfo(a)))
    }
  }
}

trait Handlers[E <: HList, M[_]] {
  def handlersMap: HMap[HandlersMapEffect]

  def select[F <: HList](select: SelectAll[E, F]): Handlers[F, M] = {
    //The original Map contains  necessarily all the subValues
    this.asInstanceOf[Handlers[F, M]]
  }

  private[effects] def transform[L[_]](fn:M ~> L): Handlers[E, L]
}

object Handlers {

  class HandlersMapEffect[K, V]

  implicit def eh[E <: EFFECT, M[_]] = new HandlersMapEffect[ClassTag[E], EffectHandler[E, M]]

  case class HnilHandlers[M[_]]() extends Handlers[HNil, M] {
    override def handlersMap: HMap[HandlersMapEffect] = HMap.empty[HandlersMapEffect]

    override private[effects] def transform[L[_]](fn: M ~> L) = HnilHandlers[L]()
  }

  case class ConsHandlers[E <: EFFECT, F <: HList, M[_]](head: EffectHandler[E, M], tail: Handlers[F, M]) extends Handlers[E :: F, M] {
    override def handlersMap: HMap[HandlersMapEffect] = tail.handlersMap + (head.effect, head)

    override private[effects] def transform[L[_]](fn: M ~> L) = {
      val mappedHead: EffectHandler[E, L] = head.transform(fn)
      val mappedTail: Handlers[F, L] = tail.transform(fn)
      ConsHandlers[E, F, L](mappedHead, mappedTail)
    }
  }

  implicit def hnilHandlers[M[_]]: Handlers[HNil, M] = new HnilHandlers[M]()

  @implicitNotFound("Could not find head or tail")
  implicit def consHandlers[E <: EFFECT, F <: HList, M[_]](implicit head: EffectHandler[E, M], tail: Handlers[F, M]): Handlers[E :: F, M] = ConsHandlers(head, tail)

}
