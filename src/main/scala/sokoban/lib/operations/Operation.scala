package sokoban.lib.operations

import sokoban.lib.Tile

import scala.util.Try

trait Operation {

  def isValidInput: Try[Unit]

  def operationBody(tilesMatrix: Array[Array[Tile]]): Try[Array[Array[Tile]]]
}
