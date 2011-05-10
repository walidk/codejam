package codejam.practice

import scala.collection.mutable.HashMap
import codejam._
import util._

class Cell(val value: Boolean, var maxSize: Int) {
  // value is true when the Cell is black
  // maxSize is the size of the largest board with lower right corner at this Cell
}

class MakingChessBoards extends ProblemSolver {
  override def solveNext(): String = {
    val line = nextIntLine()
    val M = line(0)
    val N = line(1)

    val board: Array[Array[Cell]] =
      new Array[Array[Cell]](M)

    // read board
    for (m <- 0 until M) {
      board(m) = new Array[Cell](N)
      val row = nextLine()(0)
      for (i <- 0 until row.length) {
        val cellValue = Binary.binaryList(Integer.parseInt(row(i).toString, 16)).toArray
        for (k <- 0 until 4)
          board(m)(4 * i + 3 - k) =
            new Cell(k < cellValue.length && cellValue(k) == 1, 1)
      }
    }

    // print the board
    def printState() {
      Print.printMatrix[Cell](board, (cell: Cell) => if (cell.value) "1" else "0")
      print("\n")
    }

    // compute maxSize
    for (m <- 1 until M)
      for (n <- 1 until N)
        if (
          (board(m)(n).value == board(m - 1)(n - 1).value) && // same color as top left diagonal cell
            (board(m)(n).value != board(m - 1)(n).value) && // different color than top cell
            (board(m)(n).value != board(m)(n - 1).value) // different color than left cell
        )
          board(m)(n).maxSize = 1 +
            math.min(
              board(m - 1)(n - 1).maxSize,
              math.min(board(m)(n - 1).maxSize, board(m - 1)(n).maxSize))

    // boardSize[s] is the number of boards of size s
    val boardSizes = new HashMap[Int, Int]()

    val heapElements: Array[(Int, Int, Int)] = {for(m <- 0 until M; n <- 0 until N) yield (-board(m)(n).maxSize, m, n)}.toArray
    val sizeHeap: IndexedHeap[(Int, Int, Int)] = IndexedHeap[(Int, Int, Int)](heapElements)

    var usedCells = 0
    var maxSize = 2
    while(usedCells < M*N && maxSize > 1)
    {
      val (minusMaxSize, maxM, maxN) = sizeHeap.head
      maxSize = -minusMaxSize

      // update the boardSizes HashMap
      if (boardSizes.contains(maxSize))
        boardSizes(maxSize) += 1
      else
        boardSizes += maxSize -> 1

      usedCells += maxSize * maxSize

      // update the board
      // all cells in the range
      // [maxM - maxSize, maxN - maxSize] x [maxM + maxSize, maxN + maxSize]
      // need to be updated
      for (m <- -maxSize + 1 until math.min(M - maxM, maxSize); n<- -maxSize + 1 until math.min(N - maxN, maxSize)){
        val oldMaxSize = board(maxM + m)(maxN + n).maxSize
        val newMaxSize = math.min(
          oldMaxSize,
          math.max(0, math.max(m, n)))

        if(newMaxSize < oldMaxSize){
          sizeHeap.update((-oldMaxSize, maxM+m, maxN+n), (-newMaxSize, maxM+m, maxN+n))
          board(maxM + m)(maxN + n).maxSize = newMaxSize
        }
      }
    }
    // add unused cells
    boardSizes(maxSize) += M * N - usedCells

    // result
    var result: String = boardSizes.size.toString
    for (size <- boardSizes.keys.toList.sortWith(_ > _))
      result = result + "\n" + size + " " + boardSizes(size)

    result
  }
}
