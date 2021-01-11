package projetal2020

case class Tondeuse(position: Position, currentDirection: Direction.Value) {
  def print =
    "x= " + this.position.x.toString + ", y= " + this.position.y.toString + " " + this.currentDirection.toString.toString
}
