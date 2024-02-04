package sokoban.app

import sokoban.lib.Move

import java.nio.file.{Files, Paths}
import scala.util.{Failure, Success, Try, Using}

object SolutionFile {
  def fileToMoves(filePath: String): Try[List[Move]] = {
    Using(scala.io.Source.fromFile(filePath))(f => {
      val fileContent = f.mkString.linesIterator.toList

      fileContent.map(line => Move.moveFromSymbol(line.charAt(0))).collect({
        case Failure(e) => throw e
        case Success(mv) => mv
      })
    })
  }

  def movesToFile(moves: List[Move], filePath: String): Try[Unit] = {
    val fileContent = moves.map(m => m.symbol.toString).mkString("\n")
    Try {
      Files.write(Paths.get(filePath), fileContent.getBytes())
    }
  }

}
