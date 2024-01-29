package sokoban.app

import sokoban.lib.Map

class MapFromFile(val map: Map, val fileName: String) {
  override def toString(): String = fileName
}
