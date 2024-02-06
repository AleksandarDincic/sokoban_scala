package sokoban.lib.operations

import sokoban.lib.{Floor, Tile}

import scala.util.{Success, Try}

trait AddRowOperation extends Operation {
  def addPos: RowColChangePos.Value

  override final def isValidInput: Try[Unit] = Success(())

  override final def operationBody(tilesMatrix: Array[Array[Tile]]): Try[Array[Array[Tile]]] = {
    val newRow: Array[Tile] = if (tilesMatrix.isEmpty) {
      Array()
    } else {
      Array.fill(tilesMatrix(0).length) {
        new Floor()
      }
    }
    addPos match {
      case RowColChangePos.AtStart => {
        Success(Array(newRow) ++ tilesMatrix)
      }
      case RowColChangePos.AtEnd => {
        Success(tilesMatrix ++ Array(newRow))
      }
    }
  }
}
