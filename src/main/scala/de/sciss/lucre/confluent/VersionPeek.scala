package de.sciss.lucre.confluent

object VersionPeek {
  def apply[S <: Sys[S]](acc: S#Acc): Int = acc.term.toInt
}
