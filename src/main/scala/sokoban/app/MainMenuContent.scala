package sokoban.app

import scala.swing._
import scala.swing.event.ButtonClicked

class MainMenuContent private(parent: sokoban.app.Window, val mapChoicePanelWrapper: GridPanel) extends WindowContent(parent) {
  def this(parent: sokoban.app.Window) = this(parent, new GridPanel(0, 1) {})

  val choicePanel = new MapChoicePanel(MainMenuContent.this, List())
  mapChoicePanelWrapper.contents += choicePanel

  override protected def createDisplay(): Panel = new BorderPanel {
    val titleLabel = new Label {
      text = "Sokoban"
      font = new Font(Font.Serif, Font.Bold.id, 72)
    }
    add(titleLabel, BorderPanel.Position.Center)
  }


  def addMapToDropdown(map: MapFromFile): Unit = {

    val mapChoicePanel = mapChoicePanelWrapper.contents.head.asInstanceOf[MapChoicePanel] //always safe

    mapChoicePanelWrapper.contents.clear()

    mapChoicePanelWrapper.contents += new MapChoicePanel(this, mapChoicePanel.maps.appended(map))
    mapChoicePanelWrapper.revalidate()
  }


  override protected def createMenu(): Panel = new GridPanel(0, 1) {

    vGap = 25

    contents += new Panel {
      background = new Color(100, 100, 100)
    }


    contents += mapChoicePanelWrapper

    contents += new GridPanel(0, 1) {
      vGap = 15
      val playButton = new Button("Play") {

      }
      val editButton = new Button("Edit") {

      }
      val exitButton = new Button("Exit") {
        reactions += {
          case ButtonClicked(_) => {
            parent.frame.close()
          }
        }
      }

      contents += playButton
      contents += editButton
      contents += exitButton
    }

    contents += new Panel {
      background = new Color(100, 100, 100)
    }
  }
}
