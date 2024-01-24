package sokoban

object Main extends App {
  val map = Map.mapFromFile("test.txt").get
  println(map)
  val map2 = map.move(new Up()).get
  println(map2)
  val map3 = map2.move(new Down()).get
  println(map3)
}
