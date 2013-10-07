package de.sciss

import de.sciss.file._
import de.sciss.mellite.Document
import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import de.sciss.synth.proc.Confluent

package object coelestis {
  type S = Confluent

  val sessionFile = userHome / "Desktop" / "MachinaeCoelestis" / "mellite" / "MachinaeCoelestis.mllt"

  lazy val session = Document.read(sessionFile)

  val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.US)

  implicit class RichString(s: String) {
    def toDate: Date = dateFormat.parse(s)
  }

  val firstDate = "2013-08-16 17:10:27".toDate
  val lastDate  = "2013-08-27 01:44:54".toDate
}
