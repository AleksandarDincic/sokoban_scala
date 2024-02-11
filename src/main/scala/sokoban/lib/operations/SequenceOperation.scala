package sokoban.lib.operations

import sokoban.lib.Tile

import scala.util.{Failure, Success, Try}

trait SequenceOperation extends Operation {
  def operations: List[Operation]

  override def isValidInput: Try[Unit] = Success(())

  override def operationBody(tilesMatrix: Array[Array[Tile]]): Try[Array[Array[Tile]]] = {
    val retMap = operations.zipWithIndex.foldLeft((tilesMatrix, true))((m, op) => {
      if (m._2) {
        op._1.isValidInput match {
          case Success(_) => op._1.operationBody(m._1) match {
            case Success(mm) => (mm, true)
            case Failure(e) => {
              errorCallback(new Throwable("Error at operation " + (op._2 + 1) + ": " + e.getMessage))
              (m._1, false)
            }
          }
          case Failure(e) => {
            errorCallback(new Throwable("Error at operation " + (op._2 + 1) + ": " + e.getMessage))
            (m._1, false)
          }
        }

      }
      else {
        m
      }
    })._1

    Success(retMap)
  }

  def errorCallback(e: Throwable): Unit = {}
}
