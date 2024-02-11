package sokoban.app

import sokoban.app.PlayGameContent.{INSTRUCTIONS, MOVES_PREFIX, MSG_PLAYING, MSG_SOLVING, keyToCommand}
import sokoban.lib.{Down, Left, Map, Move, Right, Solver, Up}

import java.awt.Color
import java.io.File
import javax.swing.SwingUtilities
import javax.swing.border.{Border, LineBorder}
import scala.annotation.tailrec
import scala.concurrent.Future
import scala.swing.{Alignment, BorderPanel, Button, Dialog, FileChooser, Font, GridPanel, Label, Panel}
import scala.swing.event.{ButtonClicked, Key, KeyPressed, KeyTyped, MousePressed}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class PlayGameContent private(parent: Window, map: Map, val mapPanelWrapper: GridPanel, val movesLabel: Label, val messageLabel: Label) extends WindowContent(parent) {

  def this(parent: Window, map: Map) = this(parent, map, new GridPanel(0, 1), new Label(MOVES_PREFIX + 0), new Label(""))

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
      val movesFromFileButton = new Button("Moves from File") {}
      val solveButton = new Button("Solve") {}
      val mainMenuButton = new Button("Main menu") {}
      movesFromFileButton.reactions += {
        case ButtonClicked(_) => {
          val fileChooser = new FileChooser(new File(System.getProperty("user.dir"))) {

            fileSelectionMode = FileChooser.SelectionMode.FilesOnly
            multiSelectionEnabled = false
          }
          val chooseResult = fileChooser.showDialog(PlayGameContent.this, "Select a moves file")
          if (chooseResult == FileChooser.Result.Approve) {
            SolutionFile.fileToMoves(fileChooser.selectedFile.getAbsolutePath) match {
              case Failure(e) => Dialog.showMessage(PlayGameContent.this, "Error while loading moves:\n" + e.getMessage, "Error", Dialog.Message.Error)
              case Success(moves) => {
                movesFromFileButton.enabled = false
                solveButton.enabled = false
                mainMenuButton.enabled = false
                PlayGameContent.this.enabled = false
                messageLabel.text = MSG_PLAYING

                val solverFuture = Future {
                  @tailrec
                  def playMoveTail(moves: List[Move]): Unit = moves match {
                    case Nil => ()
                    case m :: t => {
                      PlayGameContent.this.move(m) match {
                        case Some(map) => SwingUtilities.invokeLater(() => refreshMap(map))
                        case None => {}
                      }
                      Thread.sleep(PlayGameContent.DELAY_BETWEEN_MOVES)
                      playMoveTail(t)
                    }
                  }

                  playMoveTail(moves)
                }

                solverFuture.onComplete(res => {
                  movesFromFileButton.enabled = true
                  solveButton.enabled = true
                  mainMenuButton.enabled = true
                  PlayGameContent.this.enabled = true
                  PlayGameContent.this.requestFocus()
                  messageLabel.text = ""
                })
              }
            }
          }
        }
      }

      solveButton.reactions += {
        case ButtonClicked(_) => {
          movesFromFileButton.enabled = false
          solveButton.enabled = false
          mainMenuButton.enabled = false
          PlayGameContent.this.enabled = false
          messageLabel.text = MSG_SOLVING

          val solverFuture = Future {
            new Solver(PlayGameContent.this.currentMap).solve()
          }

          solverFuture.onComplete(solution => {
            movesFromFileButton.enabled = true
            solveButton.enabled = true
            mainMenuButton.enabled = true
            PlayGameContent.this.enabled = true
            PlayGameContent.this.requestFocus()
            messageLabel.text = ""
            solution match {
              case Failure(exception) => {
                Dialog.showMessage(PlayGameContent.this, "Error while solving:\n" + exception.getMessage, "Error", Dialog.Message.Error)
              }
              case Success(solution) => solution match {
                case None => Dialog.showMessage(PlayGameContent.this, "No solutions found.", "Message", Dialog.Message.Plain)
                case Some(moves) => {
                  Dialog.showMessage(PlayGameContent.this, "A solution has been found! You will now be prompted where to save it.", "Message", Dialog.Message.Plain)

                  val fileChooser = new FileChooser(new File(System.getProperty("user.dir"))) {
                    fileSelectionMode = FileChooser.SelectionMode.FilesOnly
                    multiSelectionEnabled = false
                  }
                  val chooseResult = fileChooser.showDialog(PlayGameContent.this, "Save")
                  if (chooseResult == FileChooser.Result.Approve) {
                    SolutionFile.movesToFile(moves, fileChooser.selectedFile.getAbsolutePath) match {
                      case Success(_) => {}
                      case Failure(e) => Dialog.showMessage(PlayGameContent.this, "Error while saving:\n" + e.getMessage, "Error", Dialog.Message.Error)
                    }
                  }
                }
              }
            }
          })
        }
      }

      mainMenuButton.reactions += {
        case ButtonClicked(_) => {
          parent.popContent()
        }
      }

      contents += movesFromFileButton
      contents += solveButton
      contents += mainMenuButton
    }

    contents += new BorderPanel {
      add(messageLabel, BorderPanel.Position.Center)
    }
  }

  def refreshMap(map: Map): Unit = {
    synchronized {
      mapPanelWrapper.contents.clear()
      mapPanelWrapper.contents += new MapPanel(map)

      movesLabel.text = MOVES_PREFIX + map.numberOfMoves
      revalidate()

      if (map.isWon) {
        Dialog.showMessage(this, "You win!", "Message", Dialog.Message.Plain)
        parent.popContent()
      }
    }
  }

  def currentMap: Map = {
    mapPanelWrapper.contents.head.asInstanceOf[MapPanel].map //always safe
  }

  def move(move: Move): Option[Map] = {
    currentMap.move(move)
  }

  def undo(): Option[Map] = {
    currentMap.undo()
  }

  listenTo(keys)
  reactions += {
    case KeyPressed(_, key, _, _) => {
      //println(key)
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

  val DELAY_BETWEEN_MOVES = 200

  val INSTRUCTIONS = "<html>Legend<hr>- : floor   # : wall<br>S : player  X : crate<br>. : target O : crate on target<hr>Controls<hr>WASD : Move Z : Undo"
  val MOVES_PREFIX = "Moves: "
  val MSG_SOLVING = "Solving..."
  val MSG_PLAYING = "Playing moves from file..."

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
