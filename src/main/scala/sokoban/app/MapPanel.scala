package sokoban.app

import sokoban.lib.Map

import scala.swing.GridPanel

class MapPanel(val map: Map) extends GridPanel(map.mapHeight, map.mapWidth) {
  val maxDim = if (map.mapHeight > map.mapWidth) map.mapWidth else map.mapHeight
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
