package sokoban

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try, Using}

class Map(val tilesMatrix: Array[Array[Tile]]) {


  override def toString: String = {
    val buffer: StringBuilder = new StringBuilder()

    buffer += '\n'

    for(row <- tilesMatrix) {
      for(tile <- row) {
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