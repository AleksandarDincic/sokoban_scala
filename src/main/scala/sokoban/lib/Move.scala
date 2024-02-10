package sokoban.lib

import scala.util.{Failure, Success, Try}

sealed abstract class Move {
  def movementInCoords: (Int, Int)

  def symbol: Char

}

object Move {
  def moveFromSymbol(symbol: Char): Try[Move] = symbol match {
    case Up.SYMBOL_UP => Success(Up())
    case Down.SYMBOL_DOWN => Success(Down())
    case Left.SYMBOL_LEFT => Success(Left())
    case Right.SYMBOL_RIGHT => Success(Right())
    case _ => Failure(new Throwable("Symbol is invalid: " + symbol))
  }

  def posAfterMove(pos: (Int, Int), move: Move): (Int, Int) = {
    val moveDelta = move.movementInCoords
    (pos._1 + moveDelta._1, pos._2 + moveDelta._2)
  }

  def posBeforeMove(pos: (Int, Int), move: Move): (Int, Int) = {
    val moveDelta = move.movementInCoords
    (pos._1 - moveDelta._1, pos._2 - moveDelta._2)
  }

  val allMoves: List[Move] = List(Up(), Down(), Left(), Right())
  val diagonalMoves: List[(Move, Move)] = List(
    (Up(), Left()),
    (Up(), Right()),
    (Down(), Left()),
    (Down(), Right()),
  )
}

case class Up() extends Move {
  override def movementInCoords: (Int, Int) = (-1, 0)

  override def symbol: Char = Up.SYMBOL_UP
}

object Up {
  val SYMBOL_UP: Char = 'U'
}

case class Down() extends Move {
  override def movementInCoords: (Int, Int) = (1, 0)

  override def symbol: Char = Down.SYMBOL_DOWN
}

object Down {
  val SYMBOL_DOWN: Char = 'D'
}

case class Left() extends Move {
  override def movementInCoords: (Int, Int) = (0, -1)

  override def symbol: Char = Left.SYMBOL_LEFT
}

object Left {
  val SYMBOL_LEFT: Char = 'L'
}

case class Right() extends Move {
  override def movementInCoords: (Int, Int) = (0, 1)

  override def symbol: Char = Right.SYMBOL_RIGHT
}

object Right {
  val SYMBOL_RIGHT: Char = 'R'
}