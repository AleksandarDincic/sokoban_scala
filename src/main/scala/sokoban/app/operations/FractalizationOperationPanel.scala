package sokoban.app.operations

import sokoban.lib.operations.FractalizationOperation

import scala.swing.{ComboBox, Font, GridPanel, Label, Panel, TextField}
import scala.util.{Failure, Try}

class FractalizationOperationPanel private(val inner: GridPanel) extends OperationPanel with FractalizationOperation{

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

  inner.contents += rowFieldPanel
  inner.contents += colFieldPanel
  override def operationFromNum(currNum: Int): Panel = inner

  override def row: Int = rowField.text.trim.toInt

  override def col: Int = colField.text.trim.toInt

  override def isValidInput: Try[Unit] = {
    if (rowField.text.trim.toIntOption.isEmpty || colField.text.trim.toIntOption.isEmpty) {
      Failure(new Throwable("Coordinates must be integers"))
    } else {
      super.isValidInput
    }
  }

  override def numOfOperations: Int = 1
}
