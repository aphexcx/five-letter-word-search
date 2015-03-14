package wordsearch

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

object AStar {
  type Word = String

  lazy val fiveLetterWords: Set[String] = Source.fromFile(getClass.getResource("/sgbwords.txt").getPath).getLines().toSet

  def isValid(w: Word): Boolean = fiveLetterWords.contains(w)

  // All valid words that are one letter off from the given word.
  def neighbors(w: Word): Set[Word] = {
    oneLetterOff(w).toSet intersect fiveLetterWords
  }

  // All possible words that are one letter off from the given word.
  def oneLetterOff(w: Word): Seq[Word] =
    (0 until w.length).par flatMap { i =>
      ('a' to 'z').par withFilter (_ != w(i)) map (w.updated(i, _))
    } seq

  //A list that traces the lineage of a word given an ancestry mapping.
  def reconstructPath(current: Word, came_from: Map[Word, Word]): List[Word] = {
    def go(current: Word): List[Word] = {
      if (came_from contains current)
        current :: go(came_from(current))
      else
        List(current)
    }

    go(current)
  }

  // Hamming distance; the number of letters that differ between s and t.
  def distance(s: Word, t: Word): Int = (s zip t).par count (x => x._1 != x._2)

}

import AStar.Word

case class AStar(start: Word, goal: Word) {
  import AStar._
  require(isValid(start) && isValid(goal))

  type PriorityQueue = Set[Word]

  // A* Graph Search.
  def search: List[Word] = {
    @tailrec
    def go(openset: PriorityQueue, closedset: Set[Word], parents: Map[Word, Word]): List[Word] = {
      if (openset.isEmpty) {
        reconstructPath(goal, parents) reverse
      }
      else {
        // Pull out the best word in OPEN (the node with the lowest f value) and examine it.
        val current = openset.min(Ordering.by(f(_: Word, parents)))
        
        if (current == goal) {
          // If it's the goal word, then we’re done.
          reconstructPath(goal, parents) reverse
        }
        else {
          //Mutable vars to hold modified openset and parents
          var opensetVar = openset - current
          var parentsVar = parents

          for (neighbor <- neighbors(current)) {
            val cost = g(current, parentsVar) + distance(current, neighbor)

            // if neighbor in OPEN and cost less than g(neighbor):
            if (opensetVar.contains(neighbor) && (cost < g(neighbor, parentsVar))) {
              // remove neighbor from OPEN, because new path is better
              opensetVar -= neighbor
            }
            // if neighbor not in OPEN and neighbor not in CLOSED:
            if (!opensetVar.contains(neighbor) && !closedset.contains(neighbor)) {
              // add neighbor to OPEN
              opensetVar += neighbor
              // set neighbor's parent to current
              parentsVar = parentsVar.updated(neighbor, current)
            }
          }

          go(opensetVar, closedset + current, parentsVar)
        }
      }
    }

    go(Set[Word](start),
      Set(),
      Map())
  }

  // Score of each word.
  def f(x: Word, parents: Map[Word, Word]): Int = g(x, parents) + h(x)

  // Cost from start along best known path.
  def g(x: Word, parents: Map[Word, Word]): Int = {
    reconstructPath(x, parents).length
  }

  // Heuristic to estimate how far we are from the goal word, using Hamming Distance.
  // It's impossible to get to the goal in fewer steps, since we're only changing one letter at a time
  // Therefore, this heuristic is admissible (it can never overestimate the steps).
  def h(word: Word): Int = {
    distance(word, goal)
  }

}