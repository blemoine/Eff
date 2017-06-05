package co.sachemmolo.effects

import co.sachemmolo.effects.Handlers.HandlersMapEffect
import shapeless.{::, HList, HMap, HNil}
import shapeless.ops.hlist.SelectAll

import scala.annotation.implicitNotFound
import shapeless._

trait EffectHandler[E <: EFFECT, M[_]] {
  def effect: E

  def pure[A](a: => A): M[A]
}

trait Handlers[E <: HList, M[_]] {
  def handlersMap: HMap[HandlersMapEffect]

  def select[F <: HList](select: SelectAll[E, F]): Handlers[F, M] = {
    val self = this
    new Handlers[F, M] {
      def handlersMap = self.handlersMap
    }
  }
}

object Handlers {

  class HandlersMapEffect[K, V]

  implicit def eh[E <: EFFECT, M[_]] = new HandlersMapEffect[E, EffectHandler[E, M]]


  implicit def HnilHandlers[M[_]]: Handlers[HNil, M] = new Handlers[HNil, M] {
    override def handlersMap: HMap[HandlersMapEffect] = HMap.empty[HandlersMapEffect]
  }

  @implicitNotFound("Could not find head or tail")
  implicit def ConsHandlers[E <: EFFECT, F <: HList, M[_]](implicit head: EffectHandler[E, M], tail: Handlers[F, M]): Handlers[E :: F, M] = new Handlers[E :: F, M] {
    override def handlersMap: HMap[HandlersMapEffect] = tail.handlersMap + (head.effect, head)
  }


}
