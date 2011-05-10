package util

object Binary{
  def binaryList(n: Int): List[Byte] = {
    if(n<2)
      List(n.byteValue)
    else
      (n%2).byteValue::binaryList(n/2)
  }
  
}