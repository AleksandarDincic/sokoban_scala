package sokoban.lib

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try, Using}

class Map private(val tilesMatrix: Array[Array[Tile]], val moves: List[MoveOutcome]) {

  def this(tilesMatrix: Array[Array[Tile]]) = this(tilesMatrix, List())

  private class TileCounter(val players: HashSet[(Int, Int)], val crates: HashSet[(Int, Int)], val targets: HashSet[(Int, Int)]) {
    def isValid: Try[Unit] = {
      if (players.size != 1) {
        Failure(new Throwable("There should be exactly one player on the map."))
      } else if (crates.size != targets.size) {
        Failure(new Throwable("The number of crates and targets on the map should be the same."))
      }
      else Success(())
    }

    def isWon: Boolean = crates.equals(targets)

    def playerPosition: Option[(Int, Int)] = players.headOption

    //TODO: Split Map logic and Game logic into separate classes
  }

  private def calculateTileCounter(): TileCounter = {
    @tailrec
    def calculateTileCounterTail(row: Int, col: Int, currCounter: TileCounter): TileCounter = {
      if (row >= tilesMatrix.length) {
        currCounter
      }
      else {
        val nextCoords = if ((col + 1) >= tilesMatrix(0).length) {
          (row + 1, 0)
        }
        else {
          (row, col + 1)
        }

        val currTile = tilesMatrix(row)(col)
        val nextCounter = currTile match {
          case Player(_) => new TileCounter(currCounter.players.incl((row, col)), currCounter.crates, currCounter.targets)
          case Crate(floor) => floor match {
            case Target() => new TileCounter(currCounter.players, currCounter.crates.incl((row, col)), currCounter.targets.incl((row, col)))
            case _ => new TileCounter(currCounter.players, currCounter.crates.incl((row, col)), currCounter.targets)
          }
          case Target() => new TileCounter(currCounter.players, currCounter.crates, currCounter.targets.incl((row, col)))
          case _ => currCounter
        }

        calculateTileCounterTail(nextCoords._1, nextCoords._2, nextCounter)
      }
    }

    calculateTileCounterTail(0, 0, new TileCounter(HashSet(), HashSet(), HashSet()))
  }

  private val tileCounter = calculateTileCounter()

  def mapWidth: Int = tilesMatrix(0).length

  def mapHeight: Int = tilesMatrix.length

  def players: HashSet[(Int, Int)] = tileCounter.players

  def playerPosition: Option[(Int, Int)] = tileCounter.playerPosition

  def crates: HashSet[(Int, Int)] = tileCounter.crates

  def targets: HashSet[(Int, Int)] = tileCounter.targets

  def isValid: Try[Unit] = {
    val mapIsValid: Try[Unit] = Success(()) //TODO Implement map validation
    mapIsValid match {
      case Failure(e) => Failure(e)
      case Success(_) =>
        val gameStateIsValid = tileCounter.isValid
        gameStateIsValid
    }
  }

  def isWon: Boolean = tileCounter.isWon

  def move(move: Move): Option[Map] = {
    def validPos(pos: (Int, Int)): Boolean = pos._1 >= 0 || pos._1 < mapWidth || pos._2 >= 0 || pos._2 < mapHeight

    tileCounter.playerPosition match {
      case None => None
      case Some(pos) =>
        val playerTile = tilesMatrix(pos._1)(pos._2)

        val targetPos = Move.posAfterMove(pos, move)

        if (validPos(targetPos)) {
          val steppingTile = tilesMatrix(targetPos._1)(targetPos._2)
          if (steppingTile.isTraversable) {
            val playerCurrentFloorTile = playerTile.standingOn.get // safe in all cases
            val tilesMatrixNew = tilesMatrix.map(row => row.clone())

            tilesMatrixNew(pos._1)(pos._2) = playerCurrentFloorTile
            tilesMatrixNew(targetPos._1)(targetPos._2) = new Player(steppingTile)
            Some(new Map(tilesMatrixNew, new MoveOutcome(move, false) :: moves)) //TODO optimize so it doesn't remake game state every time
          }
          else if (steppingTile.isPushable) {
            val pushedToPos = Move.posAfterMove(targetPos, move)
            val pushedToTile = tilesMatrix(pushedToPos._1)(pushedToPos._2)
            if (pushedToTile.isTraversable) {
              val playerCurrentFloorTile = playerTile.standingOn.get // safe in all cases
              val tilesMatrixNew = tilesMatrix.map(row => row.clone())

              tilesMatrixNew(pos._1)(pos._2) = playerCurrentFloorTile
              tilesMatrixNew(targetPos._1)(targetPos._2) = new Player(steppingTile.standingOn.get) //safe in all cases

              tilesMatrixNew(pushedToPos._1)(pushedToPos._2) = steppingTile.cloneWithNewFloor(pushedToTile)

              Some(new Map(tilesMatrixNew, new MoveOutcome(move, true) :: moves)) //TODO optimize so it doesn't remake game state every time
            }
            else None
          }
          else None
        }
        else None
    }
  }

  //TODO ADD CLONING TO ARRAYS! ELSE NO WORKY!!!

  def undo(): Option[Map] = {
    moves match {
      case Nil => None
      case head :: tail => {
        val tilesMatrixNew = tilesMatrix.map(row => row.clone())

        val playerPos = playerPosition.get // is always safe

        val playerEarlierPos = Move.posBeforeMove(playerPos, head.move)

        val playerEarlierFloor = tilesMatrixNew(playerEarlierPos._1)(playerEarlierPos._2)
        tilesMatrixNew(playerEarlierPos._1)(playerEarlierPos._2) = new Player(playerEarlierFloor)

        if (head.objectPushed) {
          val objCurrentPos = Move.posAfterMove(playerPos, head.move)
          val objCurrentFloor = tilesMatrixNew(objCurrentPos._1)(objCurrentPos._2).standingOn.get // is always safe

          val playerCurrentFloor = tilesMatrixNew(playerPos._1)(playerPos._2).standingOn.get // is always safe

          val objNewTile = tilesMatrixNew(objCurrentPos._1)(objCurrentPos._2).cloneWithNewFloor(playerCurrentFloor)
          tilesMatrixNew(playerPos._1)(playerPos._2) = objNewTile
          tilesMatrixNew(objCurrentPos._1)(objCurrentPos._2) = objCurrentFloor
        }

        Some(new Map(tilesMatrixNew, tail))
      }
    }
  }

  override def toString: String = {
    val buffer: StringBuilder = new StringBuilder()

    for (row <- tilesMatrix) {
      for (tile <- row) {
        buffer += tile.symbol
      }
      buffer += '\n'
    }

    buffer.toString()
  }
}

object Map {


  def mapFromFile(path: String): Try[Map] = {
    def buildTileRow(line: String): Try[Array[Tile]] = {
      @tailrec
      def buildTileRowTail(line: List[Char], buffer: ListBuffer[Tile]): Try[Array[Tile]] = line match {
        case Nil => if (buffer.isEmpty) {
          Failure(new Throwable("Empty row found"))
        } else {
          Success(buffer.toArray)
        }
        case symbol :: tail =>
          val newTile = Tile.tileFromSymbol(symbol)
          newTile match {
            case Failure(e) => Failure(e)
            case Success(tile) => buildTileRowTail(tail, buffer.addOne(tile))
          }
      }

      buildTileRowTail(line.trim.toList, ListBuffer())
    }

    def buildTileMatrix(lines: List[String]): Try[Array[Array[Tile]]] = {
      @tailrec
      def buildTileMatrixTail(lines: List[String], buffer: ListBuffer[Array[Tile]]): Try[Array[Array[Tile]]] = lines match {
        case Nil => if (buffer.isEmpty) {
          Failure(new Throwable("Map is empty"))
        } else {
          Success(buffer.toArray)
        }
        case line :: tail =>
          val newRow = buildTileRow(line)
          newRow match {
            case Failure(e) => Failure(e)
            case Success(row) =>
              if (buffer.nonEmpty && buffer.head.length != row.length) {
                Failure(new Throwable("Map is not rectangular"))
              }
              else {
                buildTileMatrixTail(tail, buffer.addOne(row))
              }
          }
      }

      buildTileMatrixTail(lines, ListBuffer())
    }

    Using(scala.io.Source.fromFile(path))(f => {
      val fileContent = f.mkString.linesIterator.toList

      val matrixTry = buildTileMatrix(fileContent)

      matrixTry match {
        case Failure(e) => throw e
        case Success(matrix) => new Map(matrix)
      }
    })

  }
}