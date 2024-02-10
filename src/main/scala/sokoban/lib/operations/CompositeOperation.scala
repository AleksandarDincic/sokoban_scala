package sokoban.lib.operations

import sokoban.lib.Tile

import scala.util.{Failure, Success, Try}

trait CompositeOperation extends Operation {

  def operations: List[Operation]

  override def isValidInput: Try[Unit] = Success(())

  override def operationBody(tilesMatrix: Array[Array[Tile]]): Try[Array[Array[Tile]]] = {
    val initTry: Try[Array[Array[Tile]]] = Success(tilesMatrix)

    operations.zipWithIndex.foldLeft(initTry)((m, op) => {
      m.flatMap(mm => {
        op._1.isValidInput match {
          case Success(_) => op._1.operationBody(mm) match {
            case Success(mmm) => Success(mmm)
            case Failure(e) => Failure(new Throwable("Error at operation " + op._2 + ": " + e.getMessage))
          }
          case Failure(e) => Failure(new Throwable("Error at operation " + op._2 + ": " + e.getMessage))
        }
      })
    })
  }
}
