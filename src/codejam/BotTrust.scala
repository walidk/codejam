import util._
package codejam {

  
class BotTrust extends ProblemSolver {

  override def solveNext(): String = {
    val sequence = nextLine()
    val N = Integer.parseInt(sequence(0))

    var lastB = 1
    var lastO = 1
    var elapsedB = 0
    var elapsedO = 0
    var elapsed = 0
    var result = 0

    for(n <- 0 until N){
      val nextButton = Integer.parseInt(sequence(2*n + 2))
      if(sequence(2*n + 1) == "O"){
        elapsed = math.max(math.abs(nextButton - lastB) - elapsedB, 0)
        result += elapsed + 1
        lastB = nextButton
        elapsedB = 0
        elapsedO += elapsed + 1
      }else{
        elapsed = math.max(math.abs(nextButton - lastO) - elapsedO, 0)
        result += elapsed + 1
        lastO = nextButton
        elapsedO = 0
        elapsedB += elapsed + 1
      }
    }

    result.toString()
  }
}

}
