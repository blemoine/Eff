package co.sachemmolo.effects.effects

import co.sachemmolo.effects.{EFFECT, Eff, OneImpurity}
import shapeless._


object TryCatch {

  implicit object EXCEPTION extends EFFECT

  type EXCEPTION = EXCEPTION.type

  def tryCatch[A](fn: => A): Eff[EXCEPTION :: HNil, A] = OneImpurity((e: EXCEPTION) => fn)

}

