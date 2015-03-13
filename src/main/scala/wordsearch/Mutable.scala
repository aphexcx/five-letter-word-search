package wordsearch

import wordsearch.Immutable.Word

import scala.collection.mutable

object Mutable extends App {

  def reconstruct_path(current: Word, came_from: mutable.Map[Word, Word]): List[Word] = {
    var cur = current
    val total_path: mutable.ListBuffer[Word] = mutable.ListBuffer(current)
    while (came_from contains cur) {
      cur = came_from(cur)
      total_path.append(cur)
    }
    total_path.toList
  }

  case class AStar(start: Word, goal: Word) {

    // cost from start along best known path.
    def g(x: Word): Int = {
      //tentative implementation using reconstruct path to figure out the cost from the start.
      reconstruct_path(x, came_from).length
    }

    // heuristic to estimate how far we are from the goal word
    def h(word: Word): Int = {
      distance(word, goal)
      //Hamming distance; the number of letters that differ from the goal word
      // It's impossible to get to the goal in fewer steps, since we're only changing one letter at a time
      // Therefore, this heuristic is admissible (it can never overestimate the steps).
    }

    // score of each word
    def f(x: Word): Int = g(x) + h(x)

    def neighbors(w: Word): IndexedSeq[Word] = ???

    def distance(first: Word, second: Word): Int = ???

    // The set of nodes already evaluated.
    val closedset: mutable.Set[Word] = mutable.Set.empty[Word]

    // The set of tentative nodes to be evaluated, initially containing the start node
    val openset: mutable.PriorityQueue[Word] = new mutable.PriorityQueue[Word]()(Ordering.by(f)) // Set[Word] = Set(start)

    // The map of navigated nodes.
    val came_from: mutable.Map[Word, Word] = mutable.Map.empty()

    val g_score: mutable.Map[Word, Int] = mutable.Map.empty()
    g_score.put(start, 0)

    val f_score: mutable.Map[Word, Int] = mutable.Map.empty()

    def search(): Option[List[Word]] = {

      while (openset.nonEmpty) {
        val current: Word = openset.dequeue() //the node in openset having the lowest f_score[] value
        if (current == goal)
          return Some(reconstruct_path(goal, came_from))

        closedset.add(current)

        neighbors(current) foreach (neighbor => {
          if (!closedset.contains(neighbor)) {
            val tentative_g = g(current) + distance(current, neighbor)

            if ((!openset.toList.contains(neighbor)) || (tentative_g < g(neighbor))) {
              came_from.put(neighbor, current)
              // g_score[neighbor] := tentative_g_score
              //            f_score[neighbor] := g_score[neighbor] + heuristic_cost_estimate(neighbor, goal)
              //            if neighbor not in openset
              //              add neighbor to openset
              g_score.put(neighbor, tentative_g)
              f_score.put(neighbor, g_score(neighbor) + h(neighbor))
              if (!openset.toList.contains(neighbor)) {
                openset.enqueue(neighbor)
              }
            }

          }
        })
      }

      None
    }
  }

}
