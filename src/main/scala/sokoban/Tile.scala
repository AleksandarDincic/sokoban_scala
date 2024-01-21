package sokoban

import sokoban.Crate.{SYMBOL_CRATE_FLOOR, SYMBOL_CRATE_TARGET}
import sokoban.Floor.SYMBOL_FLOOR
import sokoban.Player.SYMBOL_PLAYER
import sokoban.Target.SYMBOL_TARGET
import sokoban.Wall.SYMBOL_WALL

sealed abstract class Tile {
  def isTraversable: Boolean

  def isPushable: Boolean

  def standingOn: Option[Tile]

  def symbol: Char
}

object Floor {
  val SYMBOL_FLOOR: Char = '-'
}

case class Floor() extends Tile {
  override val isTraversable: Boolean = true

  override val isPushable: Boolean = false

  override val standingOn: Option[Tile] = None

  override val symbol: Char = SYMBOL_FLOOR
}

object Wall {
  val SYMBOL_WALL: Char = '#'
}

case class Wall() extends Tile {
  override val isTraversable: Boolean = false

  override val isPushable: Boolean = false

  override val standingOn: Option[Tile] = None

  override val symbol: Char = SYMBOL_WALL

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
    case Some(floor@Target()) => SYMBOL_CRATE_TARGET
    case _ => SYMBOL_CRATE_FLOOR
  }
}

object Target {
  val SYMBOL_TARGET: Char = '.'
}

case class Target() extends Tile {
  override val isTraversable: Boolean = true

  override val isPushable: Boolean = false

  override val standingOn: Option[Tile] = None

  override val symbol: Char = SYMBOL_TARGET
}

object Player {
  val SYMBOL_PLAYER: Char = 'S'
}

case class Player(floor: Tile) extends Tile {
  override val isTraversable: Boolean = false

  override val isPushable: Boolean = false

  override val standingOn: Option[Tile] = Some(floor)

  override val symbol: Char = SYMBOL_PLAYER
}