package co.sachemmolo.effects.effects

import cats.Id
import co.sachemmolo.effects.{EFFECT, Eff, EffectHandler}
import shapeless.{::, HNil}

import scala.util.Try

object TryCatch {
  implicit object EXCEPTION extends EFFECT {
    override type R = Unit
    override type DefaultFunctor[X] = Id[X]

    override def resources: R = ()
  }

  type EXCEPTION = EXCEPTION.type
  def apply[A](fn: => A): Eff[EXCEPTION :: HNil, A] = Eff[EXCEPTION, A]((_:EXCEPTION#R) => fn)

  implicit def optionHandler: EffectHandler[EXCEPTION, Option] = new EffectHandler[EXCEPTION, Option] {
    override def pure[A](a: => A): Option[A] = Try(a).toOption
  }

  implicit def tryHandler: EffectHandler[EXCEPTION, Try] = new EffectHandler[EXCEPTION, Try] {
    override def pure[A](a: => A): Try[A] = Try(a)
  }

}
