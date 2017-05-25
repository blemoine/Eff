package co.sachemmolo.effects.v2

import co.sachemmolo.effects.v2.Handler.EffectHandler
import shapeless._
import cats.implicits._

object Console {
  trait CONSOLE extends EFFECT {
    def resource: scala.Console.type
  }
  implicit object DefaultSystem extends CONSOLE {
    def resource = scala.Console
  }


  def withConsole[A]( fn: scala.Console.type => A):Eff[CONSOLE :: HNil, A] = new Eff[CONSOLE :: HNil, A] {
    override def run(e: ::[CONSOLE, HNil]): A = fn(e.head.resource)
  }

  implicit def optionHandler(implicit console: CONSOLE): EffectHandler[CONSOLE, Option] = new EffectHandler[CONSOLE, Option] {
    override def handle[A, F <: HList](eff: Eff[CONSOLE :: F, A]): Eff[F, Option[A]] = new Eff[F, Option[A]] {
      override def run(e: F): Option[A] = Some(eff.run(console :: e))
    }
  }

}