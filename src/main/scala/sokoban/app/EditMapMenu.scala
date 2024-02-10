package sokoban.app

import sokoban.app
import sokoban.app.operations.{AddRowOperationPanel, OperationFactory, OperationPanel}

import java.io.File
import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, Button, Dialog, FileChooser, GridPanel, Label, ScrollPane, TextField}
import scala.util.{Failure, Success}

class EditMapMenu(val parent: EditMapContent) extends GridPanel(0, 1) {

  val operationPanelWrapper = new GridPanel(0, 1) {}

  val operationSelectWrapper = new GridPanel(0, 1) {
    contents += new OperationSelectPanel(EditMapMenu.this, OperationFactory.basicOperationFactoryList)
  }

  val operationsBufferWrapper = new GridPanel(0, 1) {
    contents += new ScrollPane() {
      contents = new OperationBufferPanel(Nil)
    }
  }

  val bufferOperationNameField = new TextField("")
  val bufferToCompositeButton = new Button("Comp") {
    enabled = false
    reactions += {
      case ButtonClicked(_) => {
        if (bufferOperationNameField.text.trim.isEmpty) {
          Dialog.showMessage(this, "You must enter a name for the new operation", "Error", Dialog.Message.Error)
        }
        else {
          val newFactory = operationsBufferWrapper.contents.head.asInstanceOf[ScrollPane].contents.head.asInstanceOf[OperationBufferPanel].createCompositeOperationFactory(bufferOperationNameField.text)
          val currentFactories = operationSelectWrapper.contents.head.asInstanceOf[OperationSelectPanel].operations

          operationSelectWrapper.contents.clear()
          operationSelectWrapper.contents += new OperationSelectPanel(EditMapMenu.this, newFactory :: currentFactories)

          clearBuffer()
        }
      }
    }
  }
  val bufferToSequenceButton = new Button("Seq") {
    enabled = false

    reactions += {
      case ButtonClicked(_) => {
        if (bufferOperationNameField.text.trim.isEmpty) {
          Dialog.showMessage(this, "You must enter a name for the new operation", "Error", Dialog.Message.Error)
        }
        else {
          val newFactory = operationsBufferWrapper.contents.head.asInstanceOf[ScrollPane].contents.head.asInstanceOf[OperationBufferPanel].createSequenceOperationFactory(bufferOperationNameField.text)
          val currentFactories = operationSelectWrapper.contents.head.asInstanceOf[OperationSelectPanel].operations

          operationSelectWrapper.contents.clear()
          operationSelectWrapper.contents += new OperationSelectPanel(EditMapMenu.this, newFactory :: currentFactories)

          clearBuffer()
        }
      }
    }
  }
  val bufferClearButton = new Button("Clear") {
    enabled = false

    reactions += {
      case ButtonClicked(_) => {
        clearBuffer()
      }
    }
  }

  def clearBuffer(): Unit = {
    val currentBuffer = operationsBufferWrapper.contents.head.asInstanceOf[ScrollPane].contents.head.asInstanceOf[OperationBufferPanel].operationFactories
    operationsBufferWrapper.contents.clear()
    operationsBufferWrapper.contents += new ScrollPane() {
      contents = new OperationBufferPanel(Nil)
    }
    operationsBufferWrapper.validate()
    bufferButtonsEnabled(false)
  }

  def bufferButtonsEnabled(enabled: Boolean): Unit = {
    bufferToCompositeButton.enabled = enabled
    bufferToSequenceButton.enabled = enabled
    bufferClearButton.enabled = enabled
  }

  def updateOperationPanel(operation: OperationPanel): Unit = {
    operationPanelWrapper.contents.clear()
    operationPanelWrapper.contents += operation
    operationPanelWrapper.revalidate()
  }

  contents += operationSelectWrapper
  contents += new BorderPanel {
    add(operationPanelWrapper, BorderPanel.Position.Center)
    val operationButtonsPanel = new GridPanel(1, 0) {
      contents += new Button("Perform") {
        reactions += {
          case ButtonClicked(_) => {
            val operation = operationPanelWrapper.contents.head.asInstanceOf[OperationPanel]
            parent.performOperation(operation)
          }
        }
      }
      contents += new Button("Buffer") {
        reactions += {
          case ButtonClicked(_) => {
            val selectedFactory = operationSelectWrapper.contents.head.asInstanceOf[OperationSelectPanel].selectedFactory
            val currentBuffer = operationsBufferWrapper.contents.head.asInstanceOf[ScrollPane].contents.head.asInstanceOf[OperationBufferPanel].operationFactories
            operationsBufferWrapper.contents.clear()
            operationsBufferWrapper.contents += new ScrollPane() {
              contents = new OperationBufferPanel(currentBuffer.appended(selectedFactory))
            }
            operationsBufferWrapper.validate()
            bufferButtonsEnabled(true)
          }
        }
      }
    }
    add(operationButtonsPanel, BorderPanel.Position.South)
  }
  contents += new GridPanel(0, 1) {
    contents += operationsBufferWrapper
    contents += new GridPanel(0, 1) {
      contents += new GridPanel(1, 0) {
        contents += new Label("Name: ")
        contents += bufferOperationNameField
      }
      contents += new GridPanel(1, 0) {
        contents += bufferToCompositeButton
        contents += bufferToSequenceButton
        contents += bufferClearButton
      }
    }
  }
  contents += new GridPanel(0, 1) {
    contents += new Button("Verify") {
      reactions += {
        case ButtonClicked(_) => {
          val currMap = parent.currentMap
          currMap.isValid match {
            case Success(_) => Dialog.showMessage(this, "The map is valid!", "Success", Dialog.Message.Plain)
            case Failure(e) => Dialog.showMessage(this, "The map is invalid: " + e.getMessage, "Error", Dialog.Message.Error)
          }
        }
      }
    }
    contents += new Button("Save to file") {
      reactions += {
        case ButtonClicked(_) => {
          val fileChooser = new FileChooser(new File(System.getProperty("user.dir"))) {
            fileSelectionMode = FileChooser.SelectionMode.FilesOnly
            multiSelectionEnabled = false
          }
          val chooseResult = fileChooser.showDialog(EditMapMenu.this, "Save")
          if (chooseResult == FileChooser.Result.Approve) {
            val currMap = parent.currentMap
            currMap.toFile(fileChooser.selectedFile.getAbsolutePath) match {
              case Success(_) => {}
              case Failure(e) => Dialog.showMessage(EditMapMenu.this, "Error while saving:\n" + e.getMessage, "Error", Dialog.Message.Error)
            }
          }
        }
      }
    }
    contents += new Button("Main menu") {
      reactions += {
        case ButtonClicked(_) => {
          parent.backToMainMenu
        }
      }
    }
  }

}
