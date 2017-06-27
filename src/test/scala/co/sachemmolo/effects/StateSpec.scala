package co.sachemmolo.effects

import cats.data.State
import org.scalatest.{Matchers, WordSpec}
import shapeless._

class StateSpec extends WordSpec with  Matchers {

  "state eff" should {
    "work" in {

      val x: Eff[ST.STATE[Int] :: HNil, Int] = for {
        v1 <- ST.get[Int]
        _ <- ST.put[Int](3 + v1)
        v2 <- ST.get[Int]
      } yield v2

      type StateInt[Z] = State[Int, Z]

      import ST._
      implicit val stateIntHandler = EffectHandler.defaultHandler[STATE[Int]]
      x.run[StateInt].run(5).value._2 shouldBe 8
    }
  }

  object ST {
    trait STATE[S] extends EFFECT {
      type R = Unit

      type DefaultFunctor[X] = State[S, X]

      def resources = ()
    }

    implicit def DefaultState[S]:STATE[S] = new STATE[S] {}

    def get[S]: Eff[STATE[S] :: HNil, S] = Eff[STATE[S], S]( (_) => State.get[S])
    def put[S](s:S): Eff[STATE[S] :: HNil, Unit] = Eff[STATE[S], Unit]((_) => State.set(s))
  }
}