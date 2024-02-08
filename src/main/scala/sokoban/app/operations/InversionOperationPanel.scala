package sokoban.app.operations

import sokoban.lib.operations.InversionOperation

import scala.swing.{Font, GridPanel, Label, Panel}

class InversionOperationPanel extends OperationPanel with InversionOperation {
  override def operationFromNum(currNum: Int): Panel = new GridPanel(0, 1) {
    contents += new Label("No parameters") {
      font = new Font(Font.Serif, Font.Italic.id, 12)
    }
  }

  override def numOfOperations: Int = 1
}
