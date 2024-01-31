package sokoban.app

import sokoban.app.PlayGameContent.{INSTRUCTIONS, MOVES_PREFIX, keyToCommand}
import sokoban.lib.{Down, Left, Map, Move, Right, Up}

import java.awt.Color
import javax.swing.border.{Border, LineBorder}
import scala.swing.{Alignment, BorderPanel, Button, Dialog, Font, GridPanel, Label, Panel}
import scala.swing.event.{ButtonClicked, Key, KeyPressed, KeyTyped, MousePressed}

class PlayGameContent private(parent: Window, map: Map, val mapPanelWrapper: GridPanel, val movesLabel: Label) extends WindowContent(parent) {

  def this(parent: Window, map: Map) = this(parent, map, new GridPanel(0, 1), new Label(MOVES_PREFIX + 0))

  mapPanelWrapper.contents += new MapPanel(map)

  override protected def createDisplay(): Panel = mapPanelWrapper

  override protected def createMenu(): Panel = new GridPanel(0, 1) {
    contents += new Panel {}

    contents += new BorderPanel {
      val instructionsPanel = new GridPanel(0, 1) {
        val instructionsLabel = new Label(INSTRUCTIONS) {
          font = new Font(Font.Monospaced, Font.Plain.id, 10)
          horizontalAlignment = Alignment.Center
        }
        contents += instructionsLabel
        border = new LineBorder(Color.BLACK)
      }
      add(instructionsPanel, BorderPanel.Position.Center)
      add(movesLabel, BorderPanel.Position.South)
    }

    contents += new GridPanel(0, 1) {
      vGap = 15
      val movesFromFileButton = new Button("Moves from File") {

      }
      contents += movesFromFileButton
      val solveButton = new Button("Solve") {

      }
      contents += solveButton
      val mainMenuButton = new Button("Main menu") {
        reactions += {
          case ButtonClicked(_) => {
            parent.popContent()
          }
        }
      }
      contents += mainMenuButton
    }

    contents += new Panel {}
  }

  def refreshMap(map: Map): Unit = {
    mapPanelWrapper.contents.clear()
    mapPanelWrapper.contents += new MapPanel(map)

    movesLabel.text = MOVES_PREFIX + map.numberOfMoves
    revalidate()

    if (map.isWon) {
      Dialog.showMessage(this, "You win!", "Message", Dialog.Message.Plain)
      parent.popContent()
    }
  }

  def move(move: Move): Option[Map] = {
    val mapPanel = mapPanelWrapper.contents.head.asInstanceOf[MapPanel] //always safe
    mapPanel.map.move(move)
  }

  def undo(): Option[Map] = {
    val mapPanel = mapPanelWrapper.contents.head.asInstanceOf[MapPanel] //always safe
    mapPanel.map.undo()
  }

  listenTo(keys)
  reactions += {
    case KeyPressed(_, key, _, _) => {
      println(key)
      PlayGameContent.keyToCommand(key) match {
        case Some(command) => command match {
          case MoveCommand(move) => this.move(move) match {
            case Some(map) => refreshMap(map)
            case None => {}
          }
          case UndoCommand() => this.undo() match {
            case Some(map) => refreshMap(map)
            case None => {}
          }
        }
        case None => {}
      }
    }
  }
  focusable = true
  requestFocus()
}

object PlayGameContent {

  val INSTRUCTIONS = "<html>Legend<hr>- : floor   # : wall<br>S : player  X : crate<br>. : target O : crate on target<hr>Controls<hr>WASD : Move Z : Undo"
  val MOVES_PREFIX = "Moves: "

  val UP_KEY = Key.W
  val DOWN_KEY = Key.S
  val LEFT_KEY = Key.A
  val RIGHT_KEY = Key.D
  val UNDO_KEY = Key.Z

  def keyToCommand(key: Key.Value): Option[Command] = key match {
    case UP_KEY => Some(MoveCommand(Up()))
    case DOWN_KEY => Some(MoveCommand(Down()))
    case LEFT_KEY => Some(MoveCommand(Left()))
    case RIGHT_KEY => Some(MoveCommand(Right()))
    case UNDO_KEY => Some(UndoCommand())
    case _ => None
  }
}
