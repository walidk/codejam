package codejam

import util._

class GoroSort extends ProblemSolver {
    val N = 1000
    // c(n) is the expected number of hits to sort a cycle of size n
    val c = new Array[Double](N + 1)
    // r(n) is the expected number of hits to sort a random permutation of size n
    val r = new Array[Double](N + 1)

    for(n <- 2 to  N){
      r(n) = (1 + (1 to (n-1)).map(k => c(k) + r(n-k)).sum) / (n-1)
      c(n) = 1 + r(n)
    }
  
  override def solveNext(): String = {
    val N = nextIntLine()(0)
    val l = nextIntLine()
    val permutation = l.zipWithIndex.sortWith(_._1<_._1).map(_._2).toArray

    val visited = new Array[Boolean](N)
    var cycles = List[Int]()

    for(n <- 0 until N){
      var m = n
      var cycleSize = 0
      while(!visited(m)){
        visited(m) = true
        cycleSize += 1
        m = permutation(m)
      }
      if(cycleSize > 1)
        cycles = cycleSize :: cycles
    }
    
    cycles.map(c(_)).sum.toString
  }
}
