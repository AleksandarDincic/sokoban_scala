package sokoban.app
import sokoban.app.PlayGameContent.keyToMove
import sokoban.lib.{Down, Left, Map, Move, Right, Up}

import scala.swing.event.{Key, KeyPressed, KeyTyped, MousePressed}
import scala.swing.{GridPanel, Panel}

class PlayGameContent private(parent: Window, map: Map, val mapPanelWrapper: GridPanel) extends WindowContent(parent) {

  def this(parent: Window, map: Map) = this(parent, map, new GridPanel(0, 1))

  mapPanelWrapper.contents += new MapPanel(map)

  override protected def createDisplay(): Panel = mapPanelWrapper

  override protected def createMenu(): Panel = new GridPanel(0, 1){

  }

  def refreshMap(map: Map): Unit = {
    mapPanelWrapper.contents.clear()
    mapPanelWrapper.contents += new MapPanel(map)
    revalidate()
  }

  def move(move: Move): Option[Map] = {
    val mapPanel = mapPanelWrapper.contents.head.asInstanceOf[MapPanel] //always safe
    mapPanel.map.move(move)
  }

  listenTo(keys)
  reactions += {
    case KeyPressed(_, key, _, _) => {
      println(key)
      val move = keyToMove(key)
      this.move(move) match {
        case Some(map) => refreshMap(map)
        case None => {}
      }
    }
  }

  focusable = true
  requestFocus()
}

object PlayGameContent {
  val UP_KEY = Key.W
  val DOWN_KEY = Key.S
  val LEFT_KEY = Key.A
  val RIGHT_KEY = Key.D
  val UNDO_KEY = Key.Z
  def keyToMove(key: Key.Value): Move = key match {
    case UP_KEY => new Up()
    case DOWN_KEY => new Down()
    case LEFT_KEY => new Left()
    case RIGHT_KEY => new Right()
    case _ => new Up() //TODO Implement properly
  }
}
