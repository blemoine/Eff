package co.sachemmolo.effects.v2

import co.sachemmolo.effects.v2.Handler.EffectHandler
import shapeless.HNil
import shapeless._
import cats.implicits._

import scala.util.Random

object Rnd {
  trait RND extends EFFECT {
    def resource:Double
  }
  implicit object DefaultRnd extends RND {
    override def resource: Double = Random.nextDouble
  }

    def rnd[A](fn: Double => A):Eff[RND :: HNil, A] = new Eff[RND :: HNil, A] {
      override def run(e: ::[RND, HNil]): A = fn(e.head.resource)
    }


    implicit def optionHandler(implicit rnd:RND): EffectHandler[RND, Option] = new EffectHandler[RND, Option] {
      override def handle[A, F <: HList](eff: Eff[RND :: F, A]): Eff[F, Option[A]] = new Eff[F, Option[A]] {
        override def run(e: F): Option[A] = Some(eff.run(rnd :: e))
      }
    }

}