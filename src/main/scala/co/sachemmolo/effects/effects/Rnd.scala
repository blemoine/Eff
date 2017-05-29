package co.sachemmolo.effects.effects

import co.sachemmolo.effects.{EFFECT, Eff, OneImpurity}
import shapeless._

import scala.util.Random


object Rnd {

  trait RND extends EFFECT {
    def resource: Double
  }

  implicit object DefaultRnd extends RND {
    override def resource: Double = Random.nextDouble
  }

  def rnd[A](fn: Double => A): Eff[RND :: HNil, A] = OneImpurity((rnd: RND) => fn(rnd.resource))

}
