package sokoban.app

import sokoban.lib.Map

import java.io.File
import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, BoxPanel, Button, ComboBox, Dialog, FileChooser, FlowPanel, Label, Orientation}
import scala.util.{Failure, Success}

class MapChoicePanel(val parent: MainMenuContent, val maps: List[MapFromFile]) extends BorderPanel {
  private val mapsLabel = new Label("Map:")
  add(mapsLabel, BorderPanel.Position.North)

  val mapsDropdown = new ComboBox[MapFromFile](maps)
  add(mapsDropdown, BorderPanel.Position.Center)

  val loadButton = new Button("Load") {
    reactions += {
      case ButtonClicked(_) => {
        println("pressed Load button")
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
              Dialog.showMessage(this, "Failed to load the map:\n" + e.getMessage, "Failure", Dialog.Message.Error)
            }
          }
        }
      }
    }
  }
  add(loadButton, BorderPanel.Position.South)
}
