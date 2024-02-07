package sokoban.app

import sokoban.app.operations.OperationFactory

import scala.swing.event.SelectionChanged
import scala.swing.{BorderPanel, ComboBox, GridPanel, Label, Panel}

class OperationSelectPanel(parent: EditMapMenu, operations: List[OperationFactory]) extends BorderPanel {

  add(new Label("Operation:"), BorderPanel.Position.North)

  val operationsDropdown = new ComboBox[OperationFactory](operations) {
    listenTo(selection)
    reactions += {
      case SelectionChanged(_) => {
        parent.updateOperationPanel(selection.item.f())
      }
    }
  }

  val operationsDropdownPanel = new GridPanel(0, 1) {
    contents += new Panel{}
    contents += operationsDropdown
    contents += new Panel{}
  }

  add(operationsDropdownPanel, BorderPanel.Position.Center)

  parent.updateOperationPanel(operationsDropdown.selection.item.f())
}


