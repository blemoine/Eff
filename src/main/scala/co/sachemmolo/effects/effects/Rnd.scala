package co.sachemmolo.effects.effects

import cats.Monad
import co.sachemmolo.effects.Eff.Generator
import co.sachemmolo.effects.{EFFECT, Eff, EffectHandler}
import shapeless.{::, HNil}

import scala.annotation.implicitNotFound
import scala.util.Random

object Rnd {
  trait RND extends EFFECT {
    override type R = Double
  }

  implicit object DefaultRnd extends RND {
    override def resources: Double = Random.nextDouble
  }

  def rnd[A](fn: Double => A): Eff[RND :: HNil, A] = Eff(new Generator[RND, A] {
    override def apply[M[_] : Monad](e: RND, handle: EffectHandler[RND, M]): M[A] = handle.pure(fn(e.resources))
  })

  @implicitNotFound("Could not find default effect for RND")
  implicit def optionHandler(implicit e:RND): EffectHandler[RND, Option] = new EffectHandler[RND, Option] {
    override def effect: RND = e

    override def pure[A](a: => A): Option[A] = Some(a)
  }
}