package projetal2020

class TondeuseUtility {
  def displace(
      pelouse: Pelouse,
      commande: Commande.Value,
      tondeuse: Tondeuse
  ): Tondeuse = commande match {
    case Commande.A => {
      val newPosition = tondeuse.currentDirection match {
        case Direction.S =>
          Position(tondeuse.position.x, tondeuse.position.y - 1)
        case Direction.N =>
          Position(tondeuse.position.x, tondeuse.position.y + 1)
        case Direction.W =>
          Position(tondeuse.position.x - 1, tondeuse.position.y)
        case Direction.E =>
          Position(tondeuse.position.x + 1, tondeuse.position.y)
      }
      if (newPosition.y < 0 || newPosition.y > pelouse.coinSuperieur.y || newPosition.x < 0 || newPosition.x > pelouse.coinSuperieur.x)
        tondeuse
      else tondeuse.copy(position = newPosition)
    }
    case Commande.D =>
      tondeuse.copy(
        currentDirection = swingDirection(tondeuse.currentDirection)._2
      )
    case Commande.G =>
      tondeuse.copy(
        currentDirection = swingDirection(tondeuse.currentDirection)._1
      )
  }

  def swingDirection(
      direction: Direction.Value
  ): (Direction.Value, Direction.Value) = direction match {
    case Direction.S => (Direction.E, Direction.W)
    case Direction.N => (Direction.W, Direction.E)
    case Direction.W => (Direction.S, Direction.N)
    case Direction.E => (Direction.N, Direction.S)
  }
}
