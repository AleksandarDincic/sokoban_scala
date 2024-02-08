package sokoban.app.operations

import sokoban.lib.{Crate, Floor, Player, Target, Tile, Wall}

class TileFactory(val f: () => Tile, val name: String) {

  override def toString: String = name
}

object TileFactory {

  def basicTileFactoryList() : List[TileFactory] = List(
    new TileFactory(() => new Floor, "- : floor"),
    new TileFactory(() => new Wall, "# : wall"),
    new TileFactory(() => new Player(new Floor()), "S : player"),
    new TileFactory(() => new Crate(new Floor()), "X : crate"),
    new TileFactory(() => new Target(), ". : target"),
    new TileFactory(() => new Crate(new Target()), "O : crate on target"),
  )
}
