package co.sachemmolo.effects.effects

import cats.Monad
import co.sachemmolo.effects.Eff.Generator
import co.sachemmolo.effects.EffectHandler.WithDefaultEffect
import co.sachemmolo.effects.{EFFECT, Eff, EffectHandler}
import shapeless.{::, HNil}

import scala.util.Try

object TryCatch {
  implicit object EXCEPTION extends EFFECT {
    override type R = Nothing

    override def resources: R = throw new Exception("No resource")
  }

  type EXCEPTION = EXCEPTION.type
  def apply[A](fn: => A): Eff[EXCEPTION :: HNil, A] = Eff[EXCEPTION, A]((_:EXCEPTION#R) => fn)

  implicit def optionHandler: EffectHandler[EXCEPTION, Option] = new WithDefaultEffect[EXCEPTION, Option] {
    override def pure[A](a: => A): Option[A] = Try(a).toOption
  }

}
