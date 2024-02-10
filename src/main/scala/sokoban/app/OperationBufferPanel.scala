package sokoban.app

import sokoban.app.operations.{CompositeOperationPanel, OperationFactory}

import scala.swing.{GridPanel, TextArea}

class OperationBufferPanel(val operationFactories: List[OperationFactory]) extends GridPanel(0, 1) {
  val textArea = new TextArea(operationFactories.mkString("\n")) {
    editable = false
  }

  contents += textArea

  def createCompositeOperationFactory(name: String): OperationFactory = new OperationFactory(() => {
    val operations = operationFactories.map(f => f.f())
    new CompositeOperationPanel(operations)
  }, name)
}
