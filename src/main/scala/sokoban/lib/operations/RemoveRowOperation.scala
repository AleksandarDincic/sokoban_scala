package sokoban.lib.operations

import sokoban.lib.Tile

import scala.util.{Failure, Success, Try}

trait RemoveRowOperation extends Operation {
  def removePos: RowColChangePos.Value

  override final def isValidInput: Try[Unit] = Success(())

  override final def operationBody(tilesMatrix: Array[Array[Tile]]): Try[Array[Array[Tile]]] = {
    if (tilesMatrix.isEmpty) {
      Failure(new Throwable("Cannot remove row: the map is empty"))
    }
    else removePos match {
      case RowColChangePos.AtStart => {
        Success(tilesMatrix.drop(1))
      }
      case RowColChangePos.AtEnd => {
        Success(tilesMatrix.dropRight(1))
      }
    }
  }
}
