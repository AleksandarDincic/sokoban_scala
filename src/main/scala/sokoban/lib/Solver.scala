package sokoban.lib

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}

class Solver(val map: Map) {

  val pushDistances: HashMap[(Int, Int), Array[Array[Int]]] = HashMap.from(map.targets.map(pos => pos -> Array.fill(map.mapHeight) {
    Array.fill(map.mapWidth) {
      -1
    }
  }))

  private def canPull(pos: (Int, Int), move: Move): Boolean = {
    val playerCurrentPos = Move.posAfterMove(pos, move)
    if (map.traversableFloor(playerCurrentPos)) {
      val playerInitialPos = Move.posAfterMove(playerCurrentPos, move)
      map.traversableFloor(playerInitialPos)
    }
    else false
  }

  @tailrec
  private def bfsDistances(distanceMatrix: Array[Array[Int]], posQueue: List[(Int, Int)], visited: HashSet[(Int, Int)]): Unit = posQueue match {
    case Nil => ()
    case pos :: tail => {
      if (visited.contains(pos)) {
        bfsDistances(distanceMatrix, tail, visited)
      }
      else {
        val eligibleMoves = Move.allMoves.filter(move => canPull(pos, move))
        val movesFromPos = eligibleMoves.map(move => Move.posAfterMove(pos, move)).filter(p => map.traversableFloor(p)).filter(p => !visited.contains(p))
        movesFromPos.foreach(p => {
          distanceMatrix(p._1)(p._2) = distanceMatrix(pos._1)(pos._2) + 1
        })
        bfsDistances(distanceMatrix, tail ::: movesFromPos, visited + pos)
      }
    }
  }

  if (map.isValid.isSuccess) {
    for (target <- map.targets) {
      val distancesMatrix = pushDistances(target)
      distancesMatrix(target._1)(target._2) = 0
      bfsDistances(distancesMatrix, List(target), HashSet())
    }
  }

  // init precalculated data (simple deadlocks, reachability for targets maybe?)
  val simpleDeadlocks: Array[Array[Boolean]] = {
    val deadlockArr = Array.fill(map.mapHeight) {
      Array.fill(map.mapWidth) {
        false
      }
    }

    for (row <- deadlockArr.indices; col <- deadlockArr(row).indices) {
      deadlockArr(row)(col) = pushDistances.values.forall(matrix => matrix(row)(col) >= 0)
    }

    deadlockArr
  }

  protected class Push(val cratePos: (Int, Int), val moves: List[Move]) {
    override def toString: String = moves.toString
  }

  def getPossiblePushes(map: Map): List[Push] = {
    val movesMatrix: Array[Array[List[Move]]] = Array.fill(map.mapHeight) {
      Array.fill(map.mapWidth) {
        Nil
      }
    }

    def canPush(pushingPos: (Int, Int), move: Move): Boolean = {
      if (map.tileAt(pushingPos._1, pushingPos._2).isPushable) {
        val posAfterPush = Move.posAfterMove(pushingPos, move)
        map.tileAt(posAfterPush._1, posAfterPush._2).isTraversable
      }
      else false
    }

    @tailrec
    def bfsPushesTail(posQueue: List[(Int, Int)], visited: HashSet[(Int, Int)], acc: List[Push]): List[Push] = posQueue match {
      case Nil => acc
      case pos :: tail => {
        if (visited.contains(pos)) {
          bfsPushesTail(tail, visited, acc)
        }
        else {
          val posAfterMoves = Move.allMoves.map(move => (move, Move.posAfterMove(pos, move))).filter(mp => !visited.contains(mp._2))
          val traverseMoves = posAfterMoves.filter(mp => map.tileAt(mp._2._1, mp._2._2).isTraversable).map(mp => {
            movesMatrix(mp._2._1)(mp._2._2) = (mp._1 :: movesMatrix(pos._1)(pos._2))
            mp._2
          })
          val pushes = posAfterMoves.filter(mp => canPush(mp._2, mp._1)).map(mp => new Push(mp._2, (mp._1 :: movesMatrix(pos._1)(pos._2)).reverse))

          bfsPushesTail(tail ::: traverseMoves, visited + pos, pushes ::: acc)
        }
      }
    }

    bfsPushesTail(List(map.playerPosition.get), HashSet(), Nil)
  }

  class SolverState(val map: Map, val steps: Int) {

    val heuristic: Int = {
      @tailrec
      def heuristicTail(cnt: Int, crates: HashSet[(Int, Int)], targets: HashSet[(Int, Int)]): Int = {
        if (crates.isEmpty) {
          cnt
        }
        else {
          val currCrate = crates.head

          val distancesFromTarget = HashMap.from(targets.map(t => t -> pushDistances(t)(currCrate._1)(currCrate._2)))

          val reachableTargets = distancesFromTarget.filter((kv) => kv._2 >= 0)

          if (reachableTargets.isEmpty) {
            -1
          }
          else {
            val selectedTarget = distancesFromTarget.minBy(kv => kv._2)
            heuristicTail(cnt + selectedTarget._2, crates - currCrate, targets - selectedTarget._1)
          }
        }
      }

      heuristicTail(steps, map.crates, map.targets)
    }

    val deadlock: Boolean = {
      map.crates.exists((crate) => {
        !simpleDeadlocks(crate._1)(crate._2)
      })
    }

    override val hashCode: Int = (this.map.crates, this.map.playerPosition).hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case o: SolverState => {
        this.map.crates.equals(o.map.crates) && this.map.playerPosition.equals(o.map.playerPosition)
      }
      case _ => false
    }
  }

  private implicit val stateOrdering: Ordering[SolverState] = new Ordering[SolverState] {
    override def compare(x: SolverState, y: SolverState): Int = {
      y.heuristic - x.heuristic // smaller heuristic is higher priority
    }
  }

  def solve(): Option[List[Move]] = {
    this.map.isValid match {
      case Failure(_) => None
      case Success(_) => {

        @tailrec
        def solveTail(queue: mutable.PriorityQueue[SolverState], visited: HashSet[SolverState]): Option[List[Move]] = {
          if (queue.isEmpty) None
          else {
            val currentState = queue.dequeue()
            if (visited.contains(currentState)) {
              solveTail(queue, visited)
            }
            else {
              //println(currentState.map.toString + "heuristic: " + currentState.heuristic)
              //Thread.sleep(100)
              if (currentState.map.isWon) {
                Some(currentState.map.moves.map(moveOutcome => moveOutcome.move).reverse)
              }
              else {
                val newStates = getPossiblePushes(currentState.map)
                  .map(pushes => (pushes.moves.foldLeft(currentState.map)((m, mv) => m.move(mv).get), pushes.moves.size))
                  .map(ms => new SolverState(ms._1, currentState.steps + ms._2))
                  .filter(s => !visited.contains(s))
                  .filter(s => !s.deadlock)

                solveTail(queue ++= newStates, visited + currentState)
              }
            }
          }
        }

        solveTail(mutable.PriorityQueue(new SolverState(map, 0)), HashSet())
      }
    }
  }
}
