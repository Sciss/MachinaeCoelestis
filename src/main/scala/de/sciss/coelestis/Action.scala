package de.sciss.coelestis

import play.api.libs.json.SealedTraitFormat

sealed trait Action
case object Added extends Action
case object Removed extends Action
object Action {
  implicit val format = SealedTraitFormat[Action]
}
