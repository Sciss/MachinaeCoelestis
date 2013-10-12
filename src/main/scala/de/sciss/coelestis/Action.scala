package de.sciss.coelestis

import de.sciss.play.json.AutoFormat
import play.api.libs.json.Format

sealed trait Action
case object Added   extends Action
case object Removed extends Action
case object Moved   extends Action
case object Resized extends Action
case object Split   extends Action

object Action {
  implicit val format: Format[Action] = AutoFormat[Action]
}
