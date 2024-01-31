package sokoban.app

import scala.swing.{Dimension, GridPanel, MainFrame}
import sokoban.lib.Map

import scala.collection.mutable

class Window {

  val contentStack = mutable.Stack[WindowContent]()

  contentStack.push(new MainMenuContent(Window.this))

  val frame = new MainFrame {
    title = "Sokoban"
    resizable = false
    contents = contentStack.head
    minimumSize = new Dimension(800,600)

  }

  private def refreshContent(): Unit = {
    val currentContent = contentStack.head
    frame.contents = currentContent
    frame.validate()
    frame.repaint()
    currentContent.requestFocus()
  }

  def pushNewContent(content: WindowContent): Unit = {
    contentStack.push(content)
    refreshContent()
  }

  def popContent(): Unit = {
    contentStack.pop()
    refreshContent()
  }

  def setVisible(visible: Boolean): Unit = {
    frame.visible = visible
  }
}
