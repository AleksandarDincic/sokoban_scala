package sokoban

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try, Using}

class Map(val tilesMatrix: Array[Array[Tile]]) {

  private class GameState(val players: Int, val playerPosition: Option[(Int, Int)], val misplacedCrates: Int, val openTargets: Int, val placedCrates: Int) {
    def isValid: Try[Unit] = {
      if (players != 0) {
        Failure(new Throwable("There should be exactly one player on the map."))
      } else if (misplacedCrates != openTargets) {
        Failure(new Throwable("The number of crates and targets on the map should be the same."))
      }
      else Success(())
    }

    def isWon: Boolean = misplacedCrates == 0 && openTargets == 0
  }

  private def calculateGameState(): GameState = {
    @tailrec
    def calculateGameStateTail(row: Int, col: Int, currState: GameState): GameState = {
      if (row >= tilesMatrix.length) {
        currState
      }
      else {
        val nextCoords = if ((col + 1) >= tilesMatrix(0).length) {
          (row + 1, 0)
        }
        else {
          (row, col + 1)
        }

        val currTile = tilesMatrix(row)(col)
        val nextGameState = currTile match {
          case Crate(standingOn) =>
            standingOn match {
              case Floor() => new GameState(currState.players, currState.playerPosition, currState.misplacedCrates + 1, currState.openTargets, currState.placedCrates)
              case Target() => new GameState(currState.players, currState.playerPosition, currState.misplacedCrates, currState.openTargets, currState.placedCrates + 1)
            }
          case Player(_) => new GameState(currState.players + 1, Some((row, col)), currState.misplacedCrates, currState.openTargets, currState.placedCrates)
          case Target() => new GameState(currState.players, Some((row, col)), currState.misplacedCrates, currState.openTargets + 1, currState.placedCrates)
          case _ => currState
        }

        calculateGameStateTail(nextCoords._1, nextCoords._2, nextGameState)
      }
    }

    calculateGameStateTail(0, 0, new GameState(0, None, 0, 0, 0))
  }

  private val gameState = calculateGameState()

  val mapWidth: Int = tilesMatrix(0).length
  val mapHeight: Int = tilesMatrix.length


  def movePlayer(direction: Char): Map = ???

  def isValid: Try[Unit] = {
    val mapIsValid: Try[Unit] = Success(()) //TODO Implement map validation
    mapIsValid match {
      case Failure(e) => Failure(e)
      case Success(_) =>
        val gameStateIsValid = gameState.isValid
        gameStateIsValid
    }
  }

  def isWon: Boolean = gameState.isWon

  def move(direction: Int): Boolean = ???

  override def toString: String = {
    val buffer: StringBuilder = new StringBuilder()

    buffer += '\n'

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
  def tileFromSymbol(symbol: Char): Try[Tile] = symbol match {
    case Floor.SYMBOL_FLOOR => Success(Floor())
    case Wall.SYMBOL_WALL => Success(Wall())
    case Crate.SYMBOL_CRATE_FLOOR => Success(Crate(Floor()))
    case Crate.SYMBOL_CRATE_TARGET => Success(Crate(Target()))
    case Target.SYMBOL_TARGET => Success(Target())
    case Player.SYMBOL_PLAYER => Success(Player(Floor()))
    case _ => Failure(new Throwable("Symbol is invalid: " + symbol))
  }


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
          val newTile = tileFromSymbol(symbol)
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