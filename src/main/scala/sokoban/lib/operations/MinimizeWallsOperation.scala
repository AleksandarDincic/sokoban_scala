package sokoban.lib.operations

import sokoban.lib.{Crate, Floor, Move, Player, Tile, Wall}

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.util.{Failure, Success, Try}

trait MinimizeWallsOperation extends Operation {
  override def isValidInput: Try[Unit] = Success(())

  override def operationBody(tilesMatrix: Array[Array[Tile]]): Try[Array[Array[Tile]]] = {
    val players = tilesMatrix.zipWithIndex.flatMap(r => r._1.zipWithIndex.filter {
      case (Player(_), _) => true
      case _ => false
    }.map(
      t => (r._2, t._2)
    ))

    if (players.length != 1) {
      Failure(new Throwable("There must be exactly one player on the map"))
    }
    else {
      val playerPos = players(0)
      val UNREACHABLE = 0
      val REACHABLE_WALL = 1
      val REACHABLE_FLOOR = 2
      val playerReachableWall = Array.fill(tilesMatrix.length) {
        Array.fill(tilesMatrix(0).length) {
          UNREACHABLE
        }
      }

      @tailrec
      def reachableWallsTail(stack: List[(Int, Int)], visited: HashSet[(Int, Int)]): Try[Unit] = stack match {
        case Nil => Success(())
        case pos :: tail => {
          if (!tilesMatrix.indices.contains(pos._1) || !tilesMatrix(0).indices.contains(pos._2)) {
            Failure(new Throwable("The map is not enclosed"))
          }
          else {
            val traversable = tilesMatrix(pos._1)(pos._2) match {
              case Crate(floor) => floor.isTraversable
              case Player(floor) => floor.isTraversable
              case Wall() => {
                playerReachableWall(pos._1)(pos._2) = REACHABLE_WALL
                false
              }
              case t => t.isTraversable
            }
            if (!traversable) {
              reachableWallsTail(tail, visited + pos)
            }
            else {
              playerReachableWall(pos._1)(pos._2) = REACHABLE_FLOOR
              val movesFromPos = Move.allMoves.map(move => Move.posAfterMove(pos, move)).filter(p => !visited.contains(p))
              reachableWallsTail(movesFromPos ::: tail, visited + pos)
            }
          }
        }
      }

      reachableWallsTail(List(playerPos), HashSet()) match {
        case Failure(e) => Failure(e)
        case Success(_) => {
          for (row <- tilesMatrix.indices; col <- tilesMatrix(row).indices) {
            tilesMatrix(row)(col) = tilesMatrix(row)(col) match {
              case Wall() => {
                if (playerReachableWall(row)(col) == REACHABLE_WALL) {
                  val posAfterMoves = Move.allMoves
                    .map(move => Move.posAfterMove((row, col), move))
                    .filter(p => tilesMatrix.indices.contains(p._1) && tilesMatrix(0).indices.contains(p._2))
                  val (reachableFloorCount, unreachableCount) = posAfterMoves.foldLeft((0, 0))((c, p) => {
                    (if (playerReachableWall(p._1)(p._2) == REACHABLE_FLOOR) c._1 + 1 else c._1,
                      if (playerReachableWall(p._1)(p._2) == UNREACHABLE) c._2 + 1 else c._2)
                  })

                  if (reachableFloorCount > 0 && (unreachableCount > 0 || posAfterMoves.length < 4)) {
                    tilesMatrix(row)(col)
                  }
                  else {
                    new Floor()
                  }
                }
                else new Floor()
              }
              case t => t
            }
          }
          Success(tilesMatrix)
        }
      }
    }
  }
}
