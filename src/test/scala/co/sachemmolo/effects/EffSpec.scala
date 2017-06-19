package co.sachemmolo.effects

import co.sachemmolo.effects.Eff.Pure
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}
import shapeless.HNil


class EffSpec extends WordSpec with Matchers with PropertyChecks {

  "A pure Effect" should {
    "return the original value when run" in {
      forAll { (s:String) =>
        import cats.implicits._
        Pure(() => s).run[Option](HNil) shouldBe Some(s)
      }
    }
  }

}