package projetal2020

import scala.io.Source
import java.io.File

class Parser {

  def fetshPelous(file: File): Option[Pelouse] = {
    val data = Source.fromFile(file).getLines().toList
    if (!data.isEmpty) {
      val parsedLines = data(0).split(" ")
      if (parsedLines.length != 2) {
        try {
          throw DonneesIncorectesException("Too many argument for pelouse")
        } catch {
          case c: DonneesIncorectesException =>
            c.printStackTrace
        }
        None
      } else {
        println("Ok. Processing to parse pelouse")
        Some(Pelouse(Position(parsedLines(0).toInt, parsedLines(1).toInt)))
      }
    } else None
  }

  def fetshTondeuses(file: File): List[Tondeuse] = {
    val data = Source.fromFile(file).getLines().toList
    if (!data.isEmpty) {
      val parsedLines = data.zipWithIndex
        .filter {
          case (_, index) => index != 0 && index % 2 != 0
        }
        .map(_._1)
      if (parsedLines.length == 0) {
        try {
          throw DonneesIncorectesException("No tondeuses was found")
        } catch {
          case c: DonneesIncorectesException =>
            c.printStackTrace
        }
      } else println("Ok. Processing to parse tondeuses")
      parsedLines
        .filter(value => value.split(" ").length == 3)
        .map(value => {
          val params = value.split(" ")
          Tondeuse(
            Position(params(0).toInt, params(1).toInt),
            Direction.withName(params(2))
          )
        })
    } else List.empty[Tondeuse]
  }

  //  By default toInt method can handle exception, so we rewrite this method to defin our own exception
  @throws(classOf[DonneesIncorectesException])
  def toInt(s: String): Int = s.toInt

  def fetshCommandes(file: File): List[List[Commande.Value]] = {
    val data = Source.fromFile(file).getLines().toList
    if (!data.isEmpty && data.length > 2) {
      val parsedLines = data.zipWithIndex
        .filter {
          case (_, index) => index != 0 && index % 2 == 0
        }
        .map(_._1)
      if (parsedLines.length == 0) {
        try {
          throw DonneesIncorectesException("No commands was found")
        } catch {
          case c: DonneesIncorectesException =>
            c.printStackTrace
        }
      } else println("Ok. Processing to parse commands")
      parsedLines.map(
        value =>
          value.toList.map(commande => Commande.withName(commande.toString))
      )
    } else List.empty[List[Commande.Value]]
  }

}
