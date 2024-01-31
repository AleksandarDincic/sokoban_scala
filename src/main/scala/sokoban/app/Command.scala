package sokoban.app
import sokoban.lib.Move

sealed abstract class Command {

}

case class MoveCommand(move: Move) extends Command{}

case class UndoCommand() extends Command{}