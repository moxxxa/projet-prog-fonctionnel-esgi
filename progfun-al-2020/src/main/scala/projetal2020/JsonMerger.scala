package projetal2020

import play.api.libs.json._

class JsonMerger {
  def mergeTondeusesToJson(
      index: Int,
      tondeuses: List[Tondeuse],
      commandes: List[List[Commande.Value]],
      pelouse: Pelouse
  ): JsArray = tondeuses match {
    case Nil => Json.arr()
    case tondeuse :: f =>
      val tondeuseUtility = new TondeuseUtility()
      val displacedTondeuse = commandes(index).foldLeft(tondeuse)(
        (accumulator, command) =>
          tondeuseUtility.displace(pelouse, command, accumulator)
      )
      Json.obj(
        "debut" -> Json.obj(
          "point" -> Json.obj(
            "x" -> tondeuse.position.x.toString,
            "y" -> tondeuse.position.y.toString
          ),
          "direction" -> (tondeuse.currentDirection.toString)
        ),
        "instructions" -> commandes(index),
        "fin" -> Json.obj(
          "point" -> Json.obj(
            "x" -> displacedTondeuse.position.x.toString,
            "y" -> displacedTondeuse.position.y.toString
          ),
          "direction" -> displacedTondeuse.currentDirection.toString
        )
      ) +: mergeTondeusesToJson(index + 1, f, commandes, pelouse)
  }
}
