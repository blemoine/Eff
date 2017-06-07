package co.sachemmolo.effects

import cats.Monad
import co.sachemmolo.effects.Handlers.HandlersMapEffect
import shapeless.{::, HList, HMap, HNil}
import shapeless.ops.hlist.SelectAll

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

abstract class EffectHandler[E <: EFFECT : ClassTag, M[_]] {
  def effect: ClassTag[E] = implicitly[ClassTag[E]]

  def pure[A](a: => A): M[A]
}
object EffectHandler {
  def fromMonad[E <: EFFECT : ClassTag, M[_]: Monad]: EffectHandler[E, M] = new EffectHandler[E, M] {
    override def pure[A](a: => A): M[A] = implicitly[Monad[M]].pure(a)
  }
}

trait Handlers[E <: HList, M[_]] {
  def handlersMap: HMap[HandlersMapEffect]

  def select[F <: HList](select: SelectAll[E, F]): Handlers[F, M] = {
    val self = this
    new Handlers[F, M] {
      //The original Map contains  necessarily all the subValues
      def handlersMap: HMap[HandlersMapEffect] = self.handlersMap
    }
  }
}

object Handlers {

  class HandlersMapEffect[K, V]

  implicit def eh[E <: EFFECT, M[_]] = new HandlersMapEffect[ClassTag[E], EffectHandler[E, M]]


  implicit def HnilHandlers[M[_]]: Handlers[HNil, M] = new Handlers[HNil, M] {
    override def handlersMap: HMap[HandlersMapEffect] = HMap.empty[HandlersMapEffect]
  }

  @implicitNotFound("Could not find head or tail")
  implicit def ConsHandlers[E <: EFFECT, F <: HList, M[_]](implicit head: EffectHandler[E, M], tail: Handlers[F, M]): Handlers[E :: F, M] = new Handlers[E :: F, M] {
    override def handlersMap: HMap[HandlersMapEffect] = tail.handlersMap + (head.effect, head)
  }


}
