package sokoban

import scala.util.{Failure, Success, Try}

sealed abstract class Tile {
  def isTraversable: Boolean

  def isPushable: Boolean

  def standingOn: Option[Tile]

  def symbol: Char

  def cloneWithNewFloor(floor: Tile): Tile
}

object Tile {
  def tileFromSymbol(symbol: Char): Try[Tile] = symbol match {
    case Floor.SYMBOL_FLOOR => Success(Floor())
    case Wall.SYMBOL_WALL => Success(Wall())
    case Crate.SYMBOL_CRATE_FLOOR => Success(Crate(Floor()))
    case Crate.SYMBOL_CRATE_TARGET => Success(Crate(Target()))
    case Target.SYMBOL_TARGET => Success(Target())
    case Player.SYMBOL_PLAYER => Success(Player(Floor()))
    case _ => Failure(new Throwable("Symbol is invalid: " + symbol))
  }
}

object Floor {
  val SYMBOL_FLOOR: Char = '-'
}

case class Floor() extends Tile {
  override val isTraversable: Boolean = true

  override val isPushable: Boolean = false

  override val standingOn: Option[Tile] = None

  override val symbol: Char = Floor.SYMBOL_FLOOR

  override def cloneWithNewFloor(floor: Tile): Tile = new Floor()
}

object Wall {
  val SYMBOL_WALL: Char = '#'
}

case class Wall() extends Tile {
  override val isTraversable: Boolean = false

  override val isPushable: Boolean = false

  override val standingOn: Option[Tile] = None

  override val symbol: Char = Wall.SYMBOL_WALL

  override def cloneWithNewFloor(floor: Tile): Tile = new Wall()
}

object Crate {
  val SYMBOL_CRATE_FLOOR: Char = 'X'
  val SYMBOL_CRATE_TARGET: Char = 'O'
}

case class Crate(floor: Tile) extends Tile {
  override val isTraversable: Boolean = false

  override val isPushable: Boolean = true

  override val standingOn: Option[Tile] = Some(floor)

  override def symbol: Char = this.standingOn match {
    case Some(floor@Target()) => Crate.SYMBOL_CRATE_TARGET
    case _ => Crate.SYMBOL_CRATE_FLOOR
  }

  override def cloneWithNewFloor(floor: Tile): Tile = new Crate(floor)
}

object Target {
  val SYMBOL_TARGET: Char = '.'
}

case class Target() extends Tile {
  override val isTraversable: Boolean = true

  override val isPushable: Boolean = false

  override val standingOn: Option[Tile] = None

  override val symbol: Char = Target.SYMBOL_TARGET

  override def cloneWithNewFloor(floor: Tile): Tile = new Target()
}

object Player {
  val SYMBOL_PLAYER: Char = 'S'
}

case class Player(floor: Tile) extends Tile {
  override val isTraversable: Boolean = false

  override val isPushable: Boolean = false

  override val standingOn: Option[Tile] = Some(floor)

  override val symbol: Char = Player.SYMBOL_PLAYER

  override def cloneWithNewFloor(floor: Tile): Tile = new Player(floor)
}