package sokoban.app

import scala.swing.{Color, Panel}

class TestContent extends WindowContent {
  override protected def createDisplay(): Panel = new Panel {
    background = new Color(255, 0, 0)
  }

  override protected def createMenu(): Panel = new Panel {
    background = new Color(0, 0, 255)
  }
}
