package sokoban.lib.operations

import sokoban.lib.Tile
import sokoban.lib.operations.RowColChangePos.RowColChangePos

import scala.util.{Failure, Success, Try}

trait RemoveColOperations extends Operation {
  def removePos: RowColChangePos

  override final def isValidInput: Try[Unit] = Success(())

  override final def operationBody(tilesMatrix: Array[Array[Tile]]): Try[Array[Array[Tile]]] = {
    if (tilesMatrix.isEmpty || tilesMatrix(0).isEmpty) {
      Failure(new Throwable("Cannot remove row: the map is empty"))
    }
    else removePos match {
      case RowColChangePos.AtStart => {
        Success(tilesMatrix.map(row => row.drop(1)))
      }
      case RowColChangePos.AtEnd => {
        Success(tilesMatrix.map(row => row.dropRight(1)))
      }
    }
  }
}
