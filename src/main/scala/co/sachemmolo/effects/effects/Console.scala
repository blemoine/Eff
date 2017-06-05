package co.sachemmolo.effects.effects

import cats.Monad
import co.sachemmolo.effects.Eff.Generator
import co.sachemmolo.effects.{EFFECT, Eff, EffectHandler}
import shapeless.{::, HNil}

import scala.annotation.implicitNotFound

object Console {
  trait CONSOLE extends EFFECT {
    override type R = scala.Console.type
  }
  implicit object DefaultConsole extends CONSOLE {
    def resources = scala.Console
  }

  def withConsole[A]( fn: scala.Console.type => A):Eff[CONSOLE :: HNil, A] = Eff(new Generator[CONSOLE, A] {
    override def apply[M[_] : Monad](e: CONSOLE, handle: EffectHandler[CONSOLE, M]): M[A] = handle.pure(fn(e.resources))
  })

  @implicitNotFound("Could not find default effect for Console")
  implicit def consoleHandler(implicit console:CONSOLE):EffectHandler[CONSOLE, Option] = new EffectHandler[CONSOLE, Option] {
    override def effect: CONSOLE = console

    override def pure[A](a: => A): Option[A] = Some(a)
  }
}
