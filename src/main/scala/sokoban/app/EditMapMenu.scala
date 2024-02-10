package sokoban.app

import sokoban.app
import sokoban.app.operations.{AddRowOperationPanel, OperationFactory, OperationPanel}

import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, Button, GridPanel, Label, ScrollPane, TextField}

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
          //TODO fill in
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
          //TODO fill in
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
  contents += new GridPanel(0, 1)

}
