package codejam

import scala.collection.mutable._

class Magicka extends ProblemSolver {

  override def solveNext(): String = {
    val lineIterator = nextLine().iterator

    val combine = HashMap[HashSet[Char], Char]()
    val oppose = HashSet[HashSet[Char]]()

    val C = Integer.parseInt(lineIterator.next())
    for(c <- 0 until C){
      val comb = lineIterator.next().toCharArray
      combine += (HashSet[Char](comb(0), comb(1)) -> comb(2))
    }

    val D = Integer.parseInt(lineIterator.next())
    for(d <- 0 until D){
      val opp = lineIterator.next().toCharArray
      oppose += (HashSet[Char](opp(0), opp(1)))
    }

    val N = Integer.parseInt(lineIterator.next())
    val toInvoke = lineIterator.next().toCharArray
    var result = List[Char]()

    def reduceResult(r: List[Char], c: Char): List[Char] = {
      if(r.forall(element => !oppose.contains(HashSet(element, c))))
        c::r
      else
        Nil
    }

    def updateResult(r: List[Char], c: Char): List[Char] = {
      r match {
        case Nil => List(c)
        case h::t =>
          val key = HashSet(c, h)
          combine.get(key) match {
            case None => reduceResult(h::t, c)
            case Some(comb) => comb::t
          }
      }
      
    }

    for(c <- toInvoke)
      result = updateResult(result, c)

    def printList(l: List[Char]): String = {
      l match{
        case Nil => ""
        case h::Nil => h.toString()
        case h::t => h + ", " + printList(t)
      }
    }

    "[" + printList(result.reverse) + "]"
  }
}