package sokoban.app.operations

import sokoban.lib.operations.{CompositeOperation, Operation}

import scala.swing.Panel

class CompositeOperationPanel private(override val operations: List[OperationPanel], operationsFlattened: List[Panel]) extends OperationPanel with CompositeOperation {

  def this(operations: List[OperationPanel]) = this(operations, operations.flatMap(op => (1 to op.numOfOperations).map(i => op.operationFromNum(i))))


  override def operationFromNum(currNum: Int): Panel = {
    println("requesting operation " + currNum)
    operationsFlattened(currNum - 1)
  }

  override def numOfOperations: Int = operationsFlattened.size

}
