package test.v2


object Main {
  import co.sachemmolo.effects.Eff
  import co.sachemmolo.effects.effects.Rnd._
  import shapeless._
  import cats.implicits._

  val tenOrA: Eff[RND :: HNil, String] = rnd { i =>
    if (i > 0.5) {
      "10"
    } else {
      "A"
    }
  }


}