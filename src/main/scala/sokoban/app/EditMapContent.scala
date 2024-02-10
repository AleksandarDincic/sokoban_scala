package sokoban.app

import scala.swing.{Dialog, GridPanel, Panel}
import sokoban.lib.Map
import sokoban.lib.operations.Operation

import scala.util.{Failure, Success}

class EditMapContent private(parent: Window, map: Map, val mapPanelWrapper: GridPanel) extends WindowContent(parent) {

  def this(parent: Window, map: Map) = this(parent, map, new GridPanel(0, 1))

  mapPanelWrapper.contents += new MapPanel(map)

  def currentMap: Map = {
    mapPanelWrapper.contents.head.asInstanceOf[MapPanel].map //always safe
  }

  def performOperation(operation: Operation): Unit = {
    currentMap.performOperation(operation) match {
      case Success(newMap) => {
        mapPanelWrapper.contents.clear()
        mapPanelWrapper.contents += new MapPanel(newMap)
        revalidate()
      }
      case Failure(e) => {
        Dialog.showMessage(this, e.getMessage, "Error", Dialog.Message.Error)
      }
    }

  }

  def backToMainMenu: Unit = {
    parent.popContent()
  }

  override protected def createDisplay(): Panel = mapPanelWrapper

  override protected def createMenu(): Panel = new EditMapMenu(this)
}
