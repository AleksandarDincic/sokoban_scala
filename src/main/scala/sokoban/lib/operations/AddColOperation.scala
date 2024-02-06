package sokoban.lib.operations

import sokoban.lib.{Floor, Tile}

import scala.util.{Success, Try}

trait AddColOperation extends Operation {
  def addPos: RowColChangePos.Value

  override final def isValidInput: Try[Unit] = Success(())

  override final def operationBody(tilesMatrix: Array[Array[Tile]]): Try[Array[Array[Tile]]] = addPos match {
    case RowColChangePos.AtStart => {
      Success(tilesMatrix.map(row => row ++ Array(new Floor())))
    }
    case RowColChangePos.AtEnd => {
      Success(tilesMatrix.map(row => Array(new Floor()) ++ row))
    }
  }
}
