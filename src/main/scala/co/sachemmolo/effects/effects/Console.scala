package co.sachemmolo.effects.effects

import co.sachemmolo.effects.{EFFECT, Eff, OneImpurity}
import shapeless._

object Console {
  trait CONSOLE extends EFFECT {
    def resource: scala.Console.type
  }
  implicit object DefaultSystem extends CONSOLE {
    def resource = scala.Console
  }

  def withConsole[A]( fn: scala.Console.type => A):Eff[CONSOLE :: HNil, A] = OneImpurity((e:CONSOLE) => fn(e.resource))

}