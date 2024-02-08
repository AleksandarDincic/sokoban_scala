package sokoban.lib.operations

import sokoban.lib.{Crate, Floor, Target, Tile}

import scala.util.{Success, Try}

trait InversionOperation extends Operation {

  override def isValidInput: Try[Unit] = Success(())

  override def operationBody(tilesMatrix: Array[Array[Tile]]): Try[Array[Array[Tile]]] = {
    for (row <- tilesMatrix.indices) {
      for (col <- tilesMatrix.indices) {
        tilesMatrix(row)(col) match {
          case Crate(Floor()) => tilesMatrix(row)(col) = new Target()
          case Target() => tilesMatrix(row)(col) = new Crate(new Floor())
          case _ => {}
        }
      }
    }
    Success(tilesMatrix)
  }
}
