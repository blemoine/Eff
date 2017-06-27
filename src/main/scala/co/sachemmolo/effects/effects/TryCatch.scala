package co.sachemmolo.effects.effects

import co.sachemmolo.effects.{EFFECT, Eff, EffectHandler}
import shapeless.{::, HNil}

import scala.util.Try

object TryCatch {
  implicit object EXCEPTION extends EFFECT {
    override type R = Unit
    override type DefaultFunctor[X] = Try[X]

    override def resources: R = ()
  }

  type EXCEPTION = EXCEPTION.type
  def apply[A](fn: => A): Eff[EXCEPTION :: HNil, A] = Eff[EXCEPTION, A]((_:EXCEPTION#R) => Try(fn))

  import EffectHandler.TryToOption
  import cats.implicits._
  implicit def optionHandler: EffectHandler[EXCEPTION, Option] = EffectHandler.fromMonad[EXCEPTION, Option]
}
