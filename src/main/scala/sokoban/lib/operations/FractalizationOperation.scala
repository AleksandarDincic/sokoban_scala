package sokoban.lib.operations

import sokoban.lib.operations.RowColChangePos.RowColChangePos
import sokoban.lib.{Crate, Floor, Move, Player, Tile, Wall}

import scala.::
import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.collection.immutable.Nil.:::
import scala.util.{Failure, Success, Try}

trait FractalizationOperation extends Operation {

  def row: Int

  def col: Int

  override def isValidInput: Try[Unit] = if (row >= 0 && col >= 0) Success(()) else Failure(new Throwable("Tile coordinates cannot be negative"))

  class PathToReachability(val currentPos: (Int, Int), val coveredTiles: HashSet[(Int, Int)], val path: HashSet[(Int, Int)], val steps: Int) {

    override val hashCode: Int = this.currentPos.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case o: PathToReachability => {
        this.currentPos.equals(o.currentPos)
      }
      case _ => false
    }
  }

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
    else if (!tilesMatrix.indices.contains(row) || !tilesMatrix(0).indices.contains(col)) {
      Failure(new Throwable("Tile coordinates must be within the map"))
    }
    else if (tilesMatrix(row)(col) match {
      case Wall() => false
      case _ => true
    }) {
      Failure(new Throwable("The coordinates must point to a wall"))
    }
    else {
      val playerPos = players(0)
      val UNREACHABLE = 0
      val REACHABLE_WALL = 1
      val REACHABLE_FLOOR = 2
      val NEW_FLOOR = 3
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

      def pathsToReachability(): List[PathToReachability] = {
        @tailrec
        def pathsToReachabilityTail(queue: List[PathToReachability], visited: HashSet[PathToReachability], retVal: List[PathToReachability]): List[PathToReachability] = queue match {
          case Nil => retVal
          case h :: t => {
            if (visited.contains(h) || (retVal.nonEmpty && h.steps > retVal.head.steps)) {
              pathsToReachabilityTail(t, visited, retVal)
            }
            else {
              val (row, col) = h.currentPos
              if (tilesMatrix.indices.contains(row) && tilesMatrix(0).indices.contains(col) && playerReachableWall(row)(col) == REACHABLE_FLOOR) {
                val newRetVal = if (retVal.isEmpty || retVal.head.steps > h.steps) {
                  List(h)
                } else if (retVal.head.steps == h.steps) {
                  h :: retVal
                } else {
                  retVal
                }
                pathsToReachabilityTail(t, visited + h, newRetVal)
              }
              else {
                val movesFromPos =
                  Move.allMoves
                    .map(move => Move.posAfterMove(h.currentPos, move))
                    .filter(p => tilesMatrix.indices.contains(p._1) && tilesMatrix(0).indices.contains(p._2))
                    .filter(p => !h.path.contains(p))
                    .map(p => new PathToReachability(p, h.coveredTiles + p, h.path + p, h.steps + 1))
                    .filter(p => !visited.contains(p)) ::: (Move.diagonalMoves
                    .map(moves => {
                      val newPos = Move.posAfterMove(Move.posAfterMove(h.currentPos, moves._1), moves._2)
                      val newCoveredTiles = h.coveredTiles ++ HashSet(
                        Move.posAfterMove(h.currentPos, moves._1),
                        Move.posAfterMove(h.currentPos, moves._2),
                        Move.posAfterMove(Move.posAfterMove(h.currentPos, moves._1), moves._2),
                      )
                      new PathToReachability(newPos, newCoveredTiles, h.path + newPos, h.steps + 1)
                    }))
                    .filter(p => !h.path.contains(p.currentPos))
                    .filter(p => !visited.contains(p))

                pathsToReachabilityTail(t ::: movesFromPos, visited + h, retVal)
              }
            }
          }
        }

        pathsToReachabilityTail(List(new PathToReachability((row, col), HashSet((row, col)), HashSet((row, col)), 0)), HashSet(), List())
      }

      def changeInWalls(path: PathToReachability): Int = {
        path.coveredTiles.foldLeft(0)((cnt, tile) => {

          val removedWalls = if (tilesMatrix.indices.contains(tile._1)
            && tilesMatrix(0).indices.contains(tile._2)
            && playerReachableWall(tile._1)(tile._2) == REACHABLE_WALL)
            1 else 0
          val addedWalls = Move.allMoves
            .map(move => Move.posAfterMove(tile, move))
            .filter(t => !path.coveredTiles.contains(t))
            .count(t => !tilesMatrix.indices.contains(t._1)
              || !tilesMatrix(0).indices.contains(t._2)
              || (playerReachableWall(t._1)(t._2) == UNREACHABLE && (tilesMatrix(t._1)(t._2) match {
              case Wall() => false
              case _ => true
            })))

          cnt - removedWalls + addedWalls
        })
      }

      reachableWallsTail(List(playerPos), HashSet()) match {
        case Failure(e) => Failure(e)
        case Success(_) => {
          val minReachability = if (playerReachableWall(row)(col) == REACHABLE_WALL) {
            new PathToReachability((row, col), HashSet((row, col)), HashSet((row, col)), 0)
          }
          else {
            val reachabilities = pathsToReachability()
            println("BRUH!")
            reachabilities.foreach(r => {
              for (row <- tilesMatrix.indices) {
                for (col <- tilesMatrix(0).indices) {
                  print(if (r.coveredTiles.contains((row, col))) NEW_FLOOR else playerReachableWall(row)(col))
                }
                println()
              }
              println(r.coveredTiles)
              println(r.steps)
              println(changeInWalls(r))
            })
            // reachabilities.minBy(p => changeInWalls(p))
            reachabilities.minBy(p => p.coveredTiles.size)
          }
          println("MIN:")
          for (row <- tilesMatrix.indices) {
            for (col <- tilesMatrix(0).indices) {
              print(if (minReachability.coveredTiles.contains((row, col))) NEW_FLOOR else playerReachableWall(row)(col))
            }
            println()
          }
          println(minReachability.coveredTiles)
          println(minReachability.steps)
          println(changeInWalls(minReachability))

          val minCoords = minReachability.coveredTiles.reduce((t1, t2) => {
            val lesserRow = if (t1._1 < t2._1) t1._1 else t2._1
            val lesserCol = if (t1._2 < t2._2) t1._2 else t2._2
            (lesserRow, lesserCol)
          })

          val maxCoords = minReachability.coveredTiles.reduce((t1, t2) => {
            val lesserRow = if (t1._1 > t2._1) t1._1 else t2._1
            val lesserCol = if (t1._2 > t2._2) t1._2 else t2._2
            (lesserRow, lesserCol)
          })

          println("Min coords " + minCoords)
          println("Max coords " + maxCoords)

          val rowDrift = if (minCoords._1 <= 0) -minCoords._1 + 1 else 0
          val colDrift = if (minCoords._2 <= 0) -minCoords._2 + 1 else 0

          val rowsToStart = rowDrift
          val rowsToEnd = if (maxCoords._1 >= tilesMatrix.length - 1) (maxCoords._1 - tilesMatrix.length + 2) else 0
          val colsToStart = colDrift
          val colsToEnd = if (maxCoords._2 >= tilesMatrix(0).length - 1) (maxCoords._2 - tilesMatrix(0).length + 2) else 0

          println("rows to start: " + rowsToStart)
          println("rows to end: " + rowsToEnd)
          println("cols to start: " + colsToStart)
          println("cols to end: " + colsToEnd)


          val matrixRowsAddedStart = (0 until rowsToStart).foldLeft(tilesMatrix)((m, _) => new AddRowOperation {
            override def addPos: RowColChangePos = RowColChangePos.AtStart
          }.operationBody(m).get)

          val matrixRowsAddedEnd = (0 until rowsToEnd).foldLeft(matrixRowsAddedStart)((m, _) => new AddRowOperation {
            override def addPos: RowColChangePos = RowColChangePos.AtEnd
          }.operationBody(m).get)

          val matrixColsAddedStart = (0 until colsToStart).foldLeft(matrixRowsAddedEnd)((m, _) => new AddColOperation {
            override def addPos: RowColChangePos = RowColChangePos.AtStart
          }.operationBody(m).get)

          val matrixExtended = (0 until colsToEnd).foldLeft(matrixColsAddedStart)((m, _) => new AddColOperation {
            override def addPos: RowColChangePos = RowColChangePos.AtEnd
          }.operationBody(m).get)

          val fixedCoveredTiles = minReachability.coveredTiles.map(t => (t._1 + rowDrift, t._2 + colDrift))

          fixedCoveredTiles.foreach(t => {
            if (matrixExtended(t._1)(t._2) match {
              case Wall() => true
              case _ => false
            }) {
              matrixExtended(t._1)(t._2) = new Floor()
            }

            Move.allMoves
              .map(move => Move.posAfterMove(t, move))
              .filter(tt => !fixedCoveredTiles.contains(tt))
              .foreach(tt => {
                val (old_row, old_col) = (tt._1 - rowDrift, tt._2 - colDrift)
                if (!playerReachableWall.indices.contains(old_row)
                  || !playerReachableWall(0).indices.contains(old_col)
                  || (playerReachableWall(old_row)(old_col) == UNREACHABLE)) {
                  matrixExtended(tt._1)(tt._2) = new Wall()
                }
              })
          })

          Success(matrixExtended)
        }
      }
    }
  }
}
