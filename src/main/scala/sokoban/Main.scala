package sokoban

object Main extends App {
  val map = Map.mapFromFile("test.txt")
  println(map)
}
