package sokoban.app

import java.awt.CardLayout
import javax.swing.{JFrame, JPanel}
import scala.swing.{BorderPanel, Dimension, GridPanel, MainFrame}

class Window {
  val frame = new MainFrame {
    title = "Sokoban"
    resizable = false
    contents = new GridPanel(1, 1) {
      contents += new MainMenuContent(Window.this)
    }
    size = new Dimension(800, 600)

  }

  def setVisible(visible: Boolean): Unit = frame.visible = visible
}
