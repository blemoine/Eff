package co.sachemmolo.effects.effects

import java.io.PrintStream

import cats.Id
import co.sachemmolo.effects.{EFFECT, Eff, EffectHandler}
import shapeless._

import scala.annotation.implicitNotFound


object Output {

  trait OUTPUT extends EFFECT {
    override type R = PrintStream
    override type DefaultFunctor[X] = Id[X]
  }
  implicit object STD_OUT extends OUTPUT {
    override def resources: PrintStream = System.out
  }

  def printlnEff(str:String):Eff[OUTPUT :: HNil, Unit] = Eff[OUTPUT, Unit]((p:PrintStream) => p.println(str))

  import EffectHandler.IdToOption
  import cats.implicits._
  @implicitNotFound("Could not find default effect for OUTPUT")
  implicit def outputOptionHandler:EffectHandler[OUTPUT, Option] = EffectHandler.fromMonad[OUTPUT, Option]
}