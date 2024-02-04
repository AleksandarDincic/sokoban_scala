package sokoban.app

import sokoban.lib.Map

import java.io.File
import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, BoxPanel, Button, ComboBox, Dialog, FileChooser, FlowPanel, GridPanel, Label, Orientation, Panel}
import scala.util.{Failure, Success}

class MapChoicePanel(val parent: MainMenuContent, val maps: List[MapFromFile]) extends BorderPanel {
  private val mapsLabel = new Label("Map:")
  add(mapsLabel, BorderPanel.Position.North)

  val mapsDropdown = new ComboBox[MapFromFile](maps)
  val mapsDropdownPanel = new GridPanel(0, 1) {
    contents += new Panel {}
    contents += mapsDropdown
    contents += new Panel {}
  }
  add(mapsDropdownPanel, BorderPanel.Position.Center)

  val loadButton = new Button("Load from File") {
    reactions += {
      case ButtonClicked(_) => {
        val fileChooser = new FileChooser(new File(System.getProperty("user.dir"))) {

          fileSelectionMode = FileChooser.SelectionMode.FilesOnly
          multiSelectionEnabled = false
        }
        val chooseResult = fileChooser.showDialog(this, "Select a map file")
        if (chooseResult == FileChooser.Result.Approve) {
          val mapLoadingResult = Map.mapFromFile(fileChooser.selectedFile.getAbsolutePath)
          mapLoadingResult match {
            case Success(map) => {
              val mapFromFile = new MapFromFile(map, fileChooser.selectedFile.getName)
              parent.addMapToDropdown(mapFromFile)
            }
            case Failure(e) => {
              Dialog.showMessage(this, "Failed to load the map:\n" + e.getMessage, "Error", Dialog.Message.Error)
            }
          }
        }
      }
    }
  }
  add(loadButton, BorderPanel.Position.South)
}
