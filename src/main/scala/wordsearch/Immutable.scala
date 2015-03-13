package wordsearch


import scala.io.Source
import scala.language.postfixOps

object Immutable extends App {

  type Word = String

  lazy val fiveLetterWords: Set[String] = Source.fromFile(getClass.getResource("/sgbwords.txt").getPath).getLines().toSet

  def neighbors(w: Word): Set[Word] = {
    oneLetterOff(w).toSet intersect fiveLetterWords
  }

  def oneLetterOff(w: Word): Seq[Word] =
    0 until w.length flatMap { i =>
      'a' to 'z' withFilter (_ != w(i)) map (w.updated(i, _))
    }

  def distance(s: Word, t: Word): Int = s zip t count (x => x._1 != x._2)

  def reconstructPath(current: Word, came_from: Map[Word, Word]): List[Word] = {
    def go(current: Word): List[Word] = {
      if (came_from contains current)
        current :: go(came_from(current))
      else
        List(current)
    }

    go(current)
  }

  case class AStar(start: Word, goal: Word) {

    type PriorityQueue = Set[Word] // TODO: replace with scalaz.FingerTree?

    def search: List[Word] = {
      //      @annotation.tailrec //TODO to do this, need to remove recursive calls not in tail position
      def go(openset: PriorityQueue, closedset: Set[Word], parents: Map[Word, Word]): List[Word] = {
        if (openset.isEmpty) {
          reconstructPath(goal, parents)
        }
        else {
          // Pull out the best node n in OPEN (the node with the lowest f value) and examine it.
          //          implicit val cmp =
          val current = openset.min(Ordering.by(f(_: Word, parents)))

          // If n is the goal, then we’re done.
          if (current == goal) {
            reconstructPath(goal, parents)
          }
          else {
            for (neighbor <- neighbors(current)) {
              val cost = g(current, parents) + distance(current, neighbor)

              // if neighbor in OPEN and cost less than g(neighbor):
              if (openset.contains(neighbor) && (cost < g(neighbor, parents))) {
                // remove neighbor from OPEN, because new path is better
                go(openset - current - neighbor, closedset + current, parents.updated(neighbor, current))
              }
              // if neighbor not in OPEN and neighbor not in CLOSED:
              if (!openset.contains(neighbor) && !closedset.contains(neighbor)) {
                // set g(neighbor) to cost
                // add neighbor to OPEN
                // set priority queue rank to g(neighbor) + h(neighbor)
                // set neighbor's parent to current
                go(openset - current + neighbor, closedset + current, parents.updated(neighbor, current))
              }
            }
            go(openset - current, closedset + current, parents)
          }

        }

      }

      go(Set[Word](start),
        Set.empty,
        Map())
    }

    // Score of each word
    def f(x: Word, parents: Map[Word, Word]): Int = g(x, parents) + h(x)

    // Cost from start along best known path.
    def g(x: Word, parents: Map[Word, Word]): Int = {
      //Tentative implementation using reconstruct path to figure out the cost from the start.
      reconstructPath(x, parents).length
    }

    // Heuristic to estimate how far we are from the goal word
    // Hamming distance; the number of letters that differ from the goal word
    // It's impossible to get to the goal in fewer steps, since we're only changing one letter at a time
    // Therefore, this heuristic is admissible (it can never overestimate the steps).
    def h(word: Word): Int = {
      distance(word, goal)
    }


  }


}
