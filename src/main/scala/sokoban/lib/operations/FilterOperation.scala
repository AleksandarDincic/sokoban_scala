package sokoban.lib.operations

import sokoban.lib.{Floor, Tile, Wall}

import scala.util.{Failure, Success, Try}

trait FilterOperation extends Operation {
  def row: Int

  def col: Int

  def n: Int

  override def isValidInput: Try[Unit] = {
    if (row >= 0 && col >= 0) {
      if (n >= 0) {
        Success(())
      }
      else Failure(new Throwable("Distance parameter must be positive"))
    } else Failure(new Throwable("Tile coordinates cannot be negative"))
  }

  override def operationBody(tilesMatrix: Array[Array[Tile]]): Try[Array[Array[Tile]]] = {
    if (tilesMatrix.isEmpty || tilesMatrix(0).isEmpty) {
      Failure(new Throwable("The map is empty"))
    } else {
      val check = (1 to n).flatMap(i => {
        List((row - i, col), (row + i, col), (row, col - i), (row, col + i))
      }).exists(pos => {
        if (!tilesMatrix.indices.contains(pos._1) || !tilesMatrix(0).indices.contains(pos._2)) {
          false
        } else {
          tilesMatrix(pos._1)(pos._2) match {
            case Wall() => true
            case _ => false
          }
        }
      })
      tilesMatrix(row)(col) = if (check) new Floor() else tilesMatrix(row)(col)
      Success(tilesMatrix)
    }
  }
}
