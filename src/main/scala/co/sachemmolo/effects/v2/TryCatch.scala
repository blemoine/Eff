package co.sachemmolo.effects.v2

import co.sachemmolo.effects.v2.Handler.EffectHandler
import shapeless._
import cats.implicits._
import co.sachemmolo.effects.v2.TryCatch.EXCEPTION

import scala.util.Try

object TryCatch {

  object EXCEPTION extends EFFECT
  type EXCEPTION = EXCEPTION.type

  def TryCatch[A](a: => A): Eff[EXCEPTION :: HNil, A] = new Eff[EXCEPTION :: HNil, A] {
    override def run(e: ::[EXCEPTION, HNil]): A = a
  }

  implicit val optionHandler: EffectHandler[EXCEPTION, Option] = new EffectHandler[EXCEPTION, Option] {
    override def handle[A, F <: HList](eff: Eff[::[EXCEPTION, F], A]): Eff[F, Option[A]] = new Eff[F, Option[A]] {
      override def run(e: F): Option[A] = Try(eff.run(EXCEPTION :: e)).toOption
    }
  }

}