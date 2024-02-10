package sokoban.app.operations

import sokoban.lib.operations.SequenceOperation

import scala.swing.{Dialog, Panel}

class SequenceOperationPanel private(override val operations: List[OperationPanel], operationsFlattened: List[Panel]) extends OperationPanel with SequenceOperation {

  def this(operations: List[OperationPanel]) = this(operations, operations.flatMap(op => (1 to op.numOfOperations).map(i => op.operationFromNum(i))))

  override def operationFromNum(currNum: Int): Panel = {
    operationsFlattened(currNum - 1)
  }

  override def numOfOperations: Int = operationsFlattened.size

  override def errorCallback(e: Throwable): Unit = {
    Dialog.showMessage(this, e.getMessage, "Error", Dialog.Message.Error)
  }
}
