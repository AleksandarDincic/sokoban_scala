package sokoban.app.operations

import sokoban.lib.operations.FilterOperation

import scala.swing.{GridPanel, Label, Panel, TextField}
import scala.util.{Failure, Try}

class FilterOperationPanel private(val inner: GridPanel) extends OperationPanel with FilterOperation {
  def this() = this(new GridPanel(0, 1))

  val rowField = new TextField("0")
  val rowFieldPanel = new GridPanel(1, 0) {
    contents += new Label("Row:")
    contents += rowField
  }

  val colField = new TextField("0")
  val colFieldPanel = new GridPanel(1, 0) {
    contents += new Label("Column:")
    contents += colField
  }

  val nField = new TextField("0")
  val nFieldPanel = new GridPanel(1, 0) {
    contents += new Label("N:")
    contents += nField
  }

  inner.contents += rowFieldPanel
  inner.contents += colFieldPanel
  inner.contents += nFieldPanel

  override def operationFromNum(currNum: Int): Panel = inner

  override def numOfOperations: Int = 1

  override def row: Int = rowField.text.trim.toInt

  override def col: Int = colField.text.trim.toInt

  override def n: Int = nField.text.trim.toInt

  override def isValidInput: Try[Unit] = {
    if (rowField.text.trim.toIntOption.isEmpty || colField.text.trim.toIntOption.isEmpty || nField.text.trim.toIntOption.isEmpty) {
      Failure(new Throwable("Input values must be integers"))
    } else {
      super.isValidInput
    }
  }
}
