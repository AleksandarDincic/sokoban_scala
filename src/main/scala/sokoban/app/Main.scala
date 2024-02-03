package sokoban.app

import sokoban.lib._
import scala.concurrent.duration._

object Main extends App {
  //val window = new Window
  //window.setVisible(true)
  val startTime = System.nanoTime()

  val solver = new Solver(Map.mapFromFile("glasses.txt").get) //TODO FIX + set -> list sort !!!
  println(solver.solve())

  val endTime = System.nanoTime()
  val duration = Duration.fromNanos(endTime - startTime)

  println("Runtime duration: " + duration)
}
