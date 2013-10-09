package de.sciss.lucre.confluent

// work around `term` being package-private
object VersionPeek {
  def apply[S <: Sys[S]](acc: S#Acc): Int = acc.term.toInt
}
