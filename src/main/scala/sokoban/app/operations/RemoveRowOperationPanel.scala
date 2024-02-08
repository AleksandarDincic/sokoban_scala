package sokoban.app.operations

import sokoban.lib.operations.RowColChangePos.RowColChangePos
import sokoban.lib.operations.{AddRowOperation, RemoveRowOperation, RowColChangePos}

import scala.swing.{ButtonGroup, GridPanel, Panel, RadioButton}

class RemoveRowOperationPanel private(val inner: GridPanel) extends OperationPanel with RemoveRowOperation {

  def this() = this(new GridPanel(0, 1))

  val atStartButton = new RadioButton("At start") {
    selected = true
  }
  val atEndButton = new RadioButton("At end")

  val buttonGroup: ButtonGroup = new ButtonGroup(atStartButton, atEndButton)

  inner.contents += atStartButton
  inner.contents += atEndButton

  override def removePos: RowColChangePos = {
    buttonGroup.selected match {
      case Some(btn) => {
        if (btn.eq(atStartButton)) RowColChangePos.AtStart
        else RowColChangePos.AtEnd
      }
      case None => RowColChangePos.AtStart
    }
  }

  override def operationFromNum(currNum: Int): Panel = inner

  override def numOfOperations: Int = 1
}
