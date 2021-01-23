package projetal2020

import play.api.libs.json._

import java.io.{BufferedWriter, File, FileWriter}

import scala.util.Failure

object Main extends App {

  val dataFile = new File("input/input0.txt")
  val parser = new Parser()
  val pelouse = parser.fetshPelous(dataFile)
  //  consider wrapping it in `locally
  val voidResult: Any = pelouse match {
    case None => None
    case Some(pelouseWasFound) => {
      val tondeuses = parser.fetshTondeuses(dataFile)
      val commandes = parser.fetshCommandes(dataFile)
      if (commandes.length != tondeuses.length) {
        val e = DonneesIncorectesException(
          "Exception: Commands lines not matching tondeuses number"
        )
        print(e.message)
        val _ = Failure(e)
      } else println("Ok. Commandes number matchs tondeuses number")

      val jsonMerger = new JsonMerger()

      val mergedJsonTondeuses =
        jsonMerger.mergeTondeusesToJson(
          0,
          tondeuses,
          commandes,
          pelouseWasFound
        )

      val writer = new BufferedWriter(new FileWriter("output/output0.json"))
      writer.write(
        Json prettyPrint Json
          .obj(
            "limite" -> Json.obj(
              "x" -> pelouseWasFound.coinSuperieur.x.toString,
              "y" -> pelouseWasFound.coinSuperieur.y.toString
            ),
            "tondeuses" -> mergedJsonTondeuses
          )
      )
      writer.close
    }
  }
}
