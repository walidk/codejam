package util

object Print{

  def printMatrix[A](matrix: Array[Array[A]], printer: A=>String): String = {
    var result = ""
    for(row <- matrix){
      for(cell <- row)
        result = result + printer(cell) + " "
      result = result + "\n"
    }
    print(result)
    result
  }

  def printMatrix[A](matrix: Array[Array[A]]): String = {
    printMatrix(matrix, (a: A)=>a.toString())
  }

  def printArray[A](array: Array[A], printer: A=> String): String = {
    var result = ""
    for(cell <- array)
      result = result + printer(cell) + " "
    println(result)
    result
  }

  def printArray[A](array: Array[A]): String = {
    printArray(array, (a: A) => a.toString())
  }

  def printList[A](l:List[A], separator: String): String = {
    l match {
      case Nil => ""
      case h::Nil => h.toString()
      case h::t => h + separator + " " + printList (t, separator)
    }
  }
}