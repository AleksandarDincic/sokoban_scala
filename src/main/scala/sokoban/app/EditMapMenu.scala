package sokoban.app

import sokoban.app.operations.{AddRowOperationPanel, OperationFactory, OperationPanel}

import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, Button, GridPanel, Label}

class EditMapMenu(val parent: EditMapContent) extends GridPanel(0, 1) {

  val operationPanelWrapper = new GridPanel(0, 1) {}

  val operationSelectWrapper = new GridPanel(0, 1) {
    contents += new OperationSelectPanel(EditMapMenu.this, OperationFactory.basicOperationFactoryList)
  }

  def updateOperationPanel(operation: OperationPanel): Unit = {
    operationPanelWrapper.contents.clear()
    operationPanelWrapper.contents += operation
    operationPanelWrapper.revalidate()
  }

  contents += operationSelectWrapper
  contents += operationPanelWrapper
  contents += new Button("Perform") { // TODO Move to wrapper above
    reactions += {
      case ButtonClicked(_) => {
        val operation = operationPanelWrapper.contents.head.asInstanceOf[OperationPanel]
        parent.performOperation(operation)
      }
    }

  }
  contents += new GridPanel(0, 1)

}
