package sokoban.app

import sokoban.app.MapTilePanel.{COLOR_BG, COLOR_FLOOR}

import scala.swing.{Color, Font, GridPanel, Label}
import sokoban.lib.{Crate, Floor, Player, Target, Tile, Wall}

import scala.swing.event.MousePressed

class MapTilePanel(dim: Int) extends GridPanel(0, 1) {
  background = COLOR_BG
  val tileSymbolLabel = new Label(Floor.SYMBOL_FLOOR.toString) {
    foreground = COLOR_FLOOR
    val fontSize = 117 - 5 * dim
    val realFontSize: Int = if (fontSize >= 0) fontSize else 2
    font = new Font(Font.Monospaced, Font.Bold.id, realFontSize) //15x15 => 42; 9x9 => 72; diff per size = 5
  }
  contents += tileSymbolLabel

  def setTile(tile: Tile): Unit = {
    tileSymbolLabel.text = tile.symbol.toString
    tileSymbolLabel.foreground = MapTilePanel.colorFromTile(tile)
  }
}

object MapTilePanel {
  val COLOR_BG = new Color(20, 20, 20)
  val COLOR_FLOOR = new Color(40, 40, 40)
  val COLOR_WALL = new Color(87, 87, 87)
  val COLOR_PLAYER = new Color(200, 200, 200)
  val COLOR_CRATE_FLOOR = new Color(194, 158, 70)
  val COLOR_CRATE_TARGET = new Color(19, 236, 34)
  val COLOR_TARGET = new Color(236, 34, 19)

  def colorFromTile(tile: Tile): Color = tile match {
    case Wall() => COLOR_WALL
    case Floor() => COLOR_FLOOR
    case Player(_) => COLOR_PLAYER
    case Crate(Floor()) => COLOR_CRATE_FLOOR
    case Crate(Target()) => COLOR_CRATE_TARGET
    case Target() => COLOR_TARGET
  }
}