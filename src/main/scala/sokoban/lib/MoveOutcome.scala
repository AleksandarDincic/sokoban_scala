package sokoban.lib

class MoveOutcome(val move: Move, val objectPushed: Boolean) {
  override def toString: String = move.toString
}
