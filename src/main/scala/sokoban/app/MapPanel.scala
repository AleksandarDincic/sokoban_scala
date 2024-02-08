package sokoban.app

import sokoban.lib.Map

import scala.swing.{GridPanel, Label}

class MapPanel(val map: Map) extends GridPanel(
  if (map.mapHeight == 0 || map.mapWidth == 0) 0 else map.mapHeight,
  if (map.mapHeight == 0 || map.mapWidth == 0) 1 else map.mapWidth
) {
  background = MapTilePanel.COLOR_BG
  if (map.mapWidth != 0 && map.mapHeight != 0) {
    val maxDim = if (map.mapHeight < map.mapWidth) map.mapWidth else map.mapHeight
    val mapTilePanels: Array[Array[MapTilePanel]] = Array.fill(map.mapHeight) {
      Array.fill(map.mapWidth) {
        new MapTilePanel(maxDim)
      }
    }
    for (i <- mapTilePanels.indices; j <- mapTilePanels(i).indices) {
      mapTilePanels(i)(j).setTile(map.tileAt(i, j))
      contents += mapTilePanels(i)(j)
    }
  }
  else {
    contents += new Label("The map is empty") {
      foreground = MapTilePanel.COLOR_PLAYER
    }
  }
}
