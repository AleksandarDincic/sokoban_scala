package sokoban.app

import scala.swing.{Dimension, GridPanel, MainFrame}
import sokoban.lib.Map

class Window {
  val frame = new MainFrame {
    title = "Sokoban"
    resizable = false
    contents = new GridPanel(1, 1) {
      //contents += new MainMenuContent(Window.this)
      contents += new PlayGameContent(Window.this, Map.mapFromFile("bigboy.txt").get)
    }
    size = new Dimension(800, 600)
  }

  def setVisible(visible: Boolean): Unit = {
    frame.visible = visible
  }
}
