package sokoban.lib.operations

import sokoban.lib.Tile

import scala.util.{Failure, Success, Try}

trait SetTileOperation extends Operation {

  def row: Int

  def col: Int

  def tile: Tile

  override def isValidInput: Try[Unit] = if (row >= 0 && col >= 0) Success(()) else Failure(new Throwable("Tile coordinates cannot be negative"))

  override def operationBody(tilesMatrix: Array[Array[Tile]]): Try[Array[Array[Tile]]] = {
    if (tilesMatrix.isEmpty || tilesMatrix(0).isEmpty) {
      Failure(new Throwable("The map is empty"))
    } else if (row >= tilesMatrix.length || col >= tilesMatrix(0).length) {
      Failure(new Throwable("Tile coordinates must be within the map"))
    } else {
      tilesMatrix(row)(col) = tile
      Success(tilesMatrix)
    }
  }
}
