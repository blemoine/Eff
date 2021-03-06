package co.sachemmolo.effects.effects

import java.io.{BufferedReader, InputStreamReader, PrintStream}

import cats.Id
import co.sachemmolo.effects.{EFFECT, Eff, EffectHandler}
import shapeless._

import scala.annotation.implicitNotFound
import scala.util.Try


object Input {

  trait INPUT extends EFFECT {
    override type R = BufferedReader
    override type DefaultFunctor[X] = Try[X]
  }
  implicit object STD_IN extends INPUT {
    override def resources: BufferedReader = new BufferedReader(new InputStreamReader(java.lang.System.in))
  }

  def readLine():Eff[INPUT :: HNil, String] = Eff[INPUT, String]((p:BufferedReader) => Try(p.readLine()))

  import EffectHandler.TryToOption
  import cats.implicits._
  @implicitNotFound("Could not find default effect for INPUT")
  implicit def inputOptionHandler:EffectHandler[INPUT, Option] = EffectHandler.fromMonad[INPUT, Option]
}