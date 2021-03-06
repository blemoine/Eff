package co.sachemmolo.effects.effects

import cats.Id
import co.sachemmolo.effects.{EFFECT, Eff, EffectHandler}
import shapeless.{::, HNil}
import cats.implicits._

import scala.annotation.implicitNotFound
import scala.util.Random

object Rnd {
  trait RND extends EFFECT {
    override type R = Double
    override type DefaultFunctor[X] = Id[X]
  }

  implicit object DefaultRnd extends RND {
    override def resources: Double = Random.nextDouble
  }

  def rnd[A](fn: Double => A): Eff[RND :: HNil, A] = Eff[RND, A](fn)
  def rndInt(min:Int, max:Int): Eff[RND :: HNil, Int]= Eff[RND, Int](d => (d * (max - min) + min).toInt)

  import EffectHandler.IdToOption
  @implicitNotFound("Could not find default effect for RND")
  implicit def rndHandler: EffectHandler[RND, Option] = EffectHandler.fromMonad[RND, Option]
}