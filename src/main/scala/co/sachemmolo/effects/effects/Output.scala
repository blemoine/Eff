package co.sachemmolo.effects.effects

import java.io.PrintStream

import cats.Id
import co.sachemmolo.effects.{EFFECT, Eff, EffectHandler}
import shapeless._

import scala.annotation.implicitNotFound
import scala.util.Try


object Output {

  trait OUTPUT extends EFFECT {
    override type R = PrintStream
    override type DefaultFunctor[X] = Try[X]
  }
  implicit object STD_OUT extends OUTPUT {
    override def resources: PrintStream = System.out
  }

  def printlnEff(str:String):Eff[OUTPUT :: HNil, Unit] = Eff[OUTPUT, Unit]((p:PrintStream) => Try(p.println(str)))

  import EffectHandler.TryToOption
  import cats.implicits._
  @implicitNotFound("Could not find default effect for OUTPUT")
  implicit def outputOptionHandler:EffectHandler[OUTPUT, Option] = EffectHandler.fromMonad[OUTPUT, Option]
}