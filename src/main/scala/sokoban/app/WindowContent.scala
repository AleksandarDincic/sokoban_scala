package sokoban.app

import scala.swing.{Orientation, Panel, SplitPane, Dimension}
abstract class WindowContent(val parent: Window) extends SplitPane(Orientation.Vertical) {

  final val RATIO = 0.75

  protected def createDisplay(): Panel
  protected def createMenu(): Panel

  protected val display = createDisplay()
  display.preferredSize = new Dimension(0, 0)

  protected val menu = createMenu()
  menu.preferredSize = new Dimension(0, 0)

  resizeWeight = RATIO
  dividerSize = 0
  //enabled = false

  leftComponent = display
  rightComponent = menu
}
