package codejam

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.File
import java.io.FileReader
import java.io.FileWriter
import java.io.Writer
import practice._

class CodeJamSolver(input_filepath: String, output_filepath: String, solver: ProblemSolver) {
  val input_file: File = new File(input_filepath + ".in")
  val reader: BufferedReader = new BufferedReader(new FileReader(input_file))

  val output_file: File = new File(output_filepath + ".out")
  val writer: Writer = new BufferedWriter(new FileWriter(output_file))

  solver.reader = reader

  def solveAll() {
    val T: Int = Integer.parseInt(reader.readLine)
    var result: String = ""

    for (t <- 1 until T + 1) {
      result = "Case #" + t + ": " + solver.solveNext() + "\n"
      print(result)
      writer.write(result)
    }
    writer.close()
  }
}

trait ProblemSolver {
  var reader: BufferedReader = null

  def nextLine() = {
    reader.readLine.split(" ")
  }

  def nextIntLine() = {
    nextLine().map(Integer.parseInt)
  }

  def nextHexLine() = {
    nextLine().map(Integer.parseInt(_, 16))
  }

  def solveNext(): String
}

object Main {
  def main(args: Array[String]): Unit = {
    val round = "2011\\Q\\"
    val fileName =
//      "test"
      "D-large"
    new CodeJamSolver(
      "C:\\Users\\Walid\\Downloads\\" + fileName,
      "C:\\Users\\Walid\\Downloads\\gcj.out\\" + round + fileName,
      new GoroSort()
    )
      .solveAll()
  }
}
