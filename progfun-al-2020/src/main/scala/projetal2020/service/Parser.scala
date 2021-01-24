package projetal2020

import scala.io.Source
import java.io.File
import scala.util.Failure

class Parser {

  def fetshPelous(file: File): Option[Pelouse] = {
    val data = Source.fromFile(file).getLines().toList
    if (!data.isEmpty) {
      val parsedLines = data(0).split(" ")
      if (parsedLines.length != 2) {
        val e = DonneesIncorectesException(
          "Exception: Not found or Too many argument for pelouse"
        )
        println(e.message);
        val _ = Failure(e)
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
      val numberOfInValidTondeuses = parsedLines
        .filter(value => value.split(" ").length != 3)
        .length
      if (parsedLines.length == 0 || numberOfInValidTondeuses > 0) {
        val e = DonneesIncorectesException(
          "Exception: No tondeuses / bad tondeuse format was found"
        )
        println(e.message)
        val _ = Failure(e)
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
        val e = DonneesIncorectesException("Exception: No commands was found")
        print(e.message)
        val _ = Failure(e)
      } else println("Ok. Processing to parse commands")
      parsedLines.map(
        value =>
          value.toList.map(commande => Commande.withName(commande.toString))
      )
    } else List.empty[List[Commande.Value]]
  }

}
