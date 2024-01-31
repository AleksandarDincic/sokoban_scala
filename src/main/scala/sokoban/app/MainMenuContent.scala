package sokoban.app

import scala.swing._
import scala.swing.event.ButtonClicked
import scala.util.{Failure, Success}

class MainMenuContent private(parent: sokoban.app.Window, val mapChoicePanelWrapper: GridPanel) extends WindowContent(parent) {
  def this(parent: sokoban.app.Window) = this(parent, new GridPanel(0, 1) {})

  val choicePanel = new MapChoicePanel(MainMenuContent.this, List())
  mapChoicePanelWrapper.contents += choicePanel

  override protected def createDisplay(): Panel = new BorderPanel {
    val titleLabel = new Label {
      text = "Sokoban"
      font = new Font(Font.Monospaced, Font.Bold.id, 72)
    }
    add(titleLabel, BorderPanel.Position.Center)
  }

  def mapChoicePanel: MapChoicePanel = mapChoicePanelWrapper.contents.head.asInstanceOf[MapChoicePanel] //always safe

  def addMapToDropdown(map: MapFromFile): Unit = {

    val currMapChoicePanel = this.mapChoicePanel

    mapChoicePanelWrapper.contents.clear()

    mapChoicePanelWrapper.contents += new MapChoicePanel(this, currMapChoicePanel.maps.prepended(map))
    mapChoicePanelWrapper.revalidate()
  }


  override protected def createMenu(): Panel = new GridPanel(0, 1) {

    vGap = 25

    contents += new Panel {
    }


    contents += mapChoicePanelWrapper

    contents += new GridPanel(0, 1) {
      vGap = 15
      val playButton = new Button("Play") {
        reactions += {
          case ButtonClicked(_) => {
            val currMapChoicePanel = mapChoicePanel
            if (currMapChoicePanel.maps.isEmpty) {
              Dialog.showMessage(this, "You must load a map in order to play", "Error", Dialog.Message.Error)
            }
            else {
              val currMap = currMapChoicePanel.mapsDropdown.selection.item
              currMap.map.isValid match {
                case Success(_) => parent.pushNewContent(new PlayGameContent(parent, currMap.map))
                case Failure(e) => Dialog.showMessage(this, "The map is invalid: " + e.getMessage, "Error", Dialog.Message.Error)
              }

            }
          }
        }
      }
      val editButton = new Button("Edit") {

      }
      val exitButton = new Button("Exit") {
        reactions += {
          case ButtonClicked(_) => parent.frame.dispose()
        }
      }

      contents += playButton
      contents += editButton
      contents += exitButton
    }

    contents += new Panel {
    }
  }
}
