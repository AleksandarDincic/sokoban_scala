package sokoban.app.operations

import sokoban.lib.Tile
import sokoban.lib.operations.SetTileOperation

import scala.swing.{ComboBox, GridPanel, Label, Panel, TextField}
import scala.util.{Failure, Try}

class SetTileOperationPanel private(val inner: GridPanel) extends OperationPanel with SetTileOperation {
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

  val tileDropdown = new ComboBox[TileFactory](TileFactory.basicTileFactoryList())
  val tileDropdownPanel = new GridPanel(1, 0) {
    contents += new Label("Tile:")
    contents += tileDropdown
  }

  inner.contents += rowFieldPanel
  inner.contents += colFieldPanel
  inner.contents += tileDropdownPanel

  override def operationFromNum(currNum: Int): Panel = inner

  override def numOfOperations: Int = 1

  override def row: Int = rowField.text.trim.toInt

  override def col: Int = colField.text.trim.toInt

  override def tile: Tile = tileDropdown.selection.item.f()

  override def isValidInput: Try[Unit] = {
    if (rowField.text.trim.toIntOption.isEmpty || colField.text.trim.toIntOption.isEmpty) {
      Failure(new Throwable("Coordinates must be integers"))
    } else {
      super.isValidInput
    }
  }
}
