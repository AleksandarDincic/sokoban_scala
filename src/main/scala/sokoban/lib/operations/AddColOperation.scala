package sokoban.lib.operations

import sokoban.lib.operations.RowColChangePos.RowColChangePos
import sokoban.lib.{Floor, Tile}

import scala.util.{Success, Try}

trait AddColOperation extends Operation {
  def addPos: RowColChangePos

  override def isValidInput: Try[Unit] = Success(())

  override final def operationBody(tilesMatrix: Array[Array[Tile]]): Try[Array[Array[Tile]]] = addPos match {
    case RowColChangePos.AtStart => {
      Success(tilesMatrix.map(row => Array(new Floor()) ++ row))
    }
    case RowColChangePos.AtEnd => {
      Success(tilesMatrix.map(row => row ++ Array(new Floor())))
    }
  }
}
