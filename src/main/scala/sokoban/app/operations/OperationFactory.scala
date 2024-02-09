package sokoban.app.operations

class OperationFactory(val f: () => OperationPanel, val name: String) {
  override def toString: String = name
}

object OperationFactory {
  def basicOperationFactoryList: List[OperationFactory] = List(
    new OperationFactory(() => new AddRowOperationPanel(), "Add row"),
    new OperationFactory(() => new AddColOperationPanel(), "Add column"),
    new OperationFactory(() => new RemoveRowOperationPanel(), "Remove row"),
    new OperationFactory(() => new RemoveColOperationPanel(), "Remove column"),
    new OperationFactory(() => new SetTileOperationPanel(), "Set tile"),
    new OperationFactory(() => new InversionOperationPanel(), "Inversion"),
    new OperationFactory(() => new MinimizeWallsOperationPanel(), "Minimize walls"),
  )
}
