package co.sachemmolo.effects.effects

import cats.Id
import cats.implicits._
import co.sachemmolo.effects.{EFFECT, Eff, EffectHandler}
import shapeless.{::, HNil}

import scala.annotation.implicitNotFound

object Console {
  trait CONSOLE extends EFFECT {
    override type R = scala.Console.type
    override type DefaultMonad[X] = Id[X]
  }
  implicit object DefaultConsole extends CONSOLE {
    def resources = scala.Console
  }

  def withConsole[A]( fn: scala.Console.type => A):Eff[CONSOLE :: HNil, A] = Eff[CONSOLE, A](fn)

  import EffectHandler.IdToOption
  @implicitNotFound("Could not find default effect for Console")
  implicit def consoleHandler(implicit console:CONSOLE):EffectHandler[CONSOLE, Option] = EffectHandler.fromMonad[CONSOLE, Option]
}
