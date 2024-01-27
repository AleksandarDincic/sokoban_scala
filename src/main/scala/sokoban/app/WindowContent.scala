package sokoban.app

import java.awt.GridLayout
import javax.swing.{JPanel, JSplitPane}
import scala.swing.{Orientation, SplitPane, Panel}
abstract class WindowContent extends SplitPane(Orientation.Vertical) {

  final val RATIO = 0.8

  protected def createDisplay(): Panel
  protected def createMenu(): Panel

  protected val display = createDisplay()
  protected val menu = createMenu()

  leftComponent = display
  rightComponent =  menu
  resizeWeight = RATIO
  dividerSize = 0
  enabled = false
}
