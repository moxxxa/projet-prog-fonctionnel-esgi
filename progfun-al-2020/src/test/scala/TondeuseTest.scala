package projetal2020

import java.io.File

import org.scalatest.funsuite.AnyFunSuite

class TondeuseTest extends AnyFunSuite {

  //  First test using existing file
  test(
    "1st Test- the first tondeuse (x, y, d) should be equal to (1, 3, N)\n" +
      "- 1st Test- the second tondeuse (x, y, d) should be equal to (5, 5, E)"
  ) {
    val dataFile = new File("src/test/resources/case1.txt")
    val parser = new Parser()
    val pelouse = parser.fetshPelous(dataFile)
    // val voidResult: Any =
    pelouse match {
      case None =>
        try {
          throw DonneesIncorectesException("No pelouse was found")
        } catch {
          case c: DonneesIncorectesException =>
            c.printStackTrace
        }
      case Some(pelouseWasFound) => {
        val tondeuses = parser.fetshTondeuses(dataFile)
        val commandes = parser.fetshCommandes(dataFile)
        if (commandes.length != tondeuses.length) {
          try {
            throw DonneesIncorectesException(
              "Test: Commands lines not matching tondeuses number"
            )
          } catch {
            case c: DonneesIncorectesException =>
              c.printStackTrace
          }
        } else println("Test: Ok. Commandes number matchs tondeuses number")

        tondeuses.zipWithIndex.foreach {
          case (tondeuse, index) => {
            val tondeuseUtility = new TondeuseUtility()
            val displacedTondeuse = commandes(index).foldLeft(tondeuse)(
              (accumulator, command) =>
                tondeuseUtility.displace(pelouseWasFound, command, accumulator)
            )
            if (index == 0) {
              assert(1 == displacedTondeuse.position.x)
              assert(3 == displacedTondeuse.position.y)
              assert(Direction.N == displacedTondeuse.currentDirection)
            } else {
              assert(5 == displacedTondeuse.position.x)
              assert(1 == displacedTondeuse.position.y)
              assert(Direction.E == displacedTondeuse.currentDirection)
            }
          }
        }
      }
    }
  }

  //  Second test using static data from case2.txt
  test(
    "2nd Test- the first tondeuse (x, y, d) should be equal to (0, 1, X)\n" +
      "- 2nd Test- the second tondeuse (x, y, d) should be equal to (5, 6, W)"
  ) {
    //  First tondeuse
    val pelouse = Pelouse(Position(6, 6))

    val tondeuseN1 = Tondeuse(Position(1, 3), Direction.W)

    val commandsN1: Seq[Commande.Value] = Seq(
      Commande.G,
      Commande.A,
      Commande.G,
      Commande.A,
      Commande.G,
      Commande.A,
      Commande.G,
      Commande.A,
      Commande.A,
      Commande.D,
      Commande.A,
      Commande.A,
      Commande.D
    )

    val tondeuseUtility = new TondeuseUtility()

    val displacedTondeuseN1 = commandsN1.foldLeft(tondeuseN1)(
      (accumulator, command) =>
        tondeuseUtility.displace(pelouse, command, accumulator)
    )

    assert(0 == displacedTondeuseN1.position.x)
    assert(5 == displacedTondeuseN1.position.y)
    val _ = assert(Direction.E == displacedTondeuseN1.currentDirection)

    //  Second tondeuse
    val tondeuseN2 = Tondeuse(Position(2, 5), Direction.E)
    val commandsN2: Seq[Commande.Value] = Seq(
      Commande.A,
      Commande.A,
      Commande.D,
      Commande.A,
      Commande.A,
      Commande.D,
      Commande.A,
      Commande.D,
      Commande.D,
      Commande.A,
      Commande.G,
      Commande.D,
      Commande.A
    )
    val displacedTondeuseN2 = commandsN2.foldLeft(tondeuseN2)(
      (accumulator, command) =>
        tondeuseUtility.displace(pelouse, command, accumulator)
    )

    assert(5 == displacedTondeuseN2.position.x)
    assert(3 == displacedTondeuseN2.position.y)
    //  to avoid this error :discarded non-Unit value => we need to asign the expression into a value
    val _ = assert(Direction.E == displacedTondeuseN2.currentDirection)

  }

  test(
    "3rd Test- the first tondeuse (x, y, d) should be equal to (1, 2, S)\n" +
      "- 3rd Test- the second tondeuse (x, y, d) should be equal to (0, 0, S)"
  ) {
    //  First tondeuse
    val pelouse = Pelouse(Position(7, 7))

    val tondeuseN1 = Tondeuse(Position(1, 1), Direction.N)

    val commandsN1: Seq[Commande.Value] = Seq(
      Commande.G,
      Commande.A,
      Commande.G,
      Commande.A,
      Commande.G,
      Commande.A,
      Commande.G,
      Commande.A,
      Commande.A,
      Commande.A,
      Commande.G,
      Commande.A,
      Commande.G,
      Commande.A,
      Commande.A
    )

    val tondeuseUtility = new TondeuseUtility()

    val displacedTondeuseN1 = commandsN1.foldLeft(tondeuseN1)(
      (accumulator, command) =>
        tondeuseUtility.displace(pelouse, command, accumulator)
    )

    assert(0 == displacedTondeuseN1.position.x)
    assert(1 == displacedTondeuseN1.position.y)
    assert(Direction.S == displacedTondeuseN1.currentDirection)

    //  Second tondeuse
    val tondeuseN2 = Tondeuse(Position(2, 2), Direction.W)
    val commandsN2: Seq[Commande.Value] = Seq(
      Commande.A,
      Commande.A,
      Commande.D,
      Commande.A,
      Commande.A,
      Commande.D,
      Commande.A,
      Commande.D,
      Commande.D,
      Commande.A,
      Commande.A,
      Commande.A,
      Commande.D,
      Commande.A,
      Commande.A
    )
    val displacedTondeuseN2 = commandsN2.foldLeft(tondeuseN2)(
      (accumulator, command) =>
        tondeuseUtility.displace(pelouse, command, accumulator)
    )

    assert(0 == displacedTondeuseN2.position.x)
    assert(6 == displacedTondeuseN2.position.y)
    //  to avoid this error :discarded non-Unit value => we need to asign the expression into a value
    val _ = assert(Direction.N == displacedTondeuseN2.currentDirection)

  }
}
