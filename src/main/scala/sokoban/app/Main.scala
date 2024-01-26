package sokoban.app

import sokoban.lib.{Left, Map, Up}

object Main extends App {
  val map = Map.mapFromFile("test.txt").get
  println(map.isValid)
  println(map)
  val map2 = map.move(new Up()).get
  println(map2)
  val map4 = map2.undo().get
  println(map4)
  val map5 = map4.move(new Left()).get
  println(map5)
}
