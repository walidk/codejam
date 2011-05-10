import util._
package codejam {

  
class CandySplitting extends ProblemSolver {

  override def solveNext(): String = {
    val N = nextIntLine()(0)
    val candy = nextIntLine().toList.sortWith(_<_)
    val candyXor = candy.reduceLeft(_^_)
    if(candyXor == 0) candy.tail.sum.toString else "NO"
  }
}

}
