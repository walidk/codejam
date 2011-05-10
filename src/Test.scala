import scala.util.Random
import util._

object Test
{
  def executionTime(f: ()=>Unit){
    val startTime = System.currentTimeMillis()
    f()
    val endTime = System.currentTimeMillis()
    println("executed in " + (endTime - startTime) + " ms")
  }

  def treeTest(){
    val tree = Node(0, List(Node(1), Node(2)))
    println(tree.filter(_ < 2))
    println(tree)
  }

  def heapTest(){
    val rand = new Random()
    val elements = new Array[Int](10000)
    for(i <- 0 until 10000)
      elements(i) = rand.nextInt()
    val heap = IndexedHeap[Int](elements)

    executionTime(()=>{
      while(heap.count > 0)
        heap.dequeue()
    })
    }

  def main(args: Array[String]){
    heapTest()
    treeTest()
  }
}