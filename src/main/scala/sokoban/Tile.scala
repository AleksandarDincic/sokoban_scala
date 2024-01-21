package sokoban

sealed abstract class Tile {
  def isTraversable: Boolean
  def isPushable: Boolean
  def standingOn: Option[Tile]
}

case class Floor() extends Tile {
  override val isTraversable: Boolean = true

  override val isPushable: Boolean = false

  override val standingOn: Option[Tile] = None
}

case class Wall() extends Tile {
  override val isTraversable: Boolean = false

  override val isPushable: Boolean = false

  override val standingOn: Option[Tile] = None
}

case class Crate(floor: Tile) extends Tile {
  override val isTraversable: Boolean = false

  override val isPushable: Boolean = true

  override val standingOn: Option[Tile] = Some(floor)
}

case class Target() extends Tile {
  override val isTraversable: Boolean = true

  override val isPushable: Boolean = false

  override val standingOn: Option[Tile] = None
}

case class Player(floor: Tile) extends Tile {
  override val isTraversable: Boolean = false

  override val isPushable: Boolean = false

  override val standingOn: Option[Tile] = Some(floor)
}