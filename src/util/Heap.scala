package util

import collection.mutable.HashMap

class Heap[A](var size: Int = 10)(implicit manifest: Manifest[A], converter: A=>Ordered[A]) {
  var count = 0
  var elements: Array[A] = new Array[A](size)

  def doubleSize() {
    size *= 2
    val newElements = new Array[A](size)
    for(i <- 0 until size/2)
      newElements(i) = elements(i)
    elements = newElements
  }

  def parent(i: Int) = (i-1)/2
  def leftChild(i: Int) = 2*i + 1
  def rightChild(i: Int) = 2*i + 2

  def swap(i: Int, j: Int) {
    val a = elements(i)
    elements(i) = elements(j)
    elements(j) = a
  }

  def percolateDown(i: Int){
    val right = rightChild(i)
    val left = leftChild(i)
    var min = i
    if(left < count && elements(left) < elements(min))
      min = left
    if(right < count && elements(right) < elements(min))
      min = right
    if(min != i){
      swap(min, i)
      percolateDown(min)
    }
  }

  def percolateUp(i: Int){
    val p = parent(i)
    if(elements(i) < elements(p)){
      swap(p, i)
      percolateUp(p)
    }
  }

  def add(element: A){
    count += 1
    if(count > size)
      doubleSize()
    elements(count-1) = element

    percolateUp(count-1)
  }

  def head: A = elements(0)

  def dequeue(): A = {
    val root = elements(0)
    swap(0, count-1)
    count -= 1
    percolateDown(0)
    root
  }

  private def find(element: A, index: Int): Option[Int] = {
    if(index >= count || elements(index) > element)
      None
    else if(elements(index) == element)
      Some(index)
    else find(element, leftChild(index)) match {
      case None => find(element, rightChild(index))
      case Some(i) => Some(i)
    }
  }

  def remove(element: A) {
    find(element, 0) match{
      case Some(i) => {
        swap(i, count-1)
        count -= 1
        percolateUp(i)
        percolateDown(i)
      }
      case None => ()
    }
  }

  def update(element: A, newElement: A) {
    find(element, 0) match{
      case Some(i) => {
        elements(i) = newElement
      if(newElement < element)
        percolateUp(i)
      else
        percolateDown(i)
      }
      case None => ()
    }
  }

}

class IndexedHeap[A](size: Int)(implicit manifest: Manifest[A], converter: A=>Ordered[A]) extends Heap[A](size){
  val indexes: HashMap[A, Int] = new HashMap[A, Int]()

  override def swap(i: Int, j: Int){
    super.swap(i, j)
    indexes(elements(i)) = i
    indexes(elements(j)) = j
  }

  override def add(element: A){
    indexes(element) = count
    super.add(element)
  }

  override def dequeue(): A = {
    indexes.remove(elements(0))
    super.dequeue()
  }

  override def remove(element: A) {
    val i = indexes(element)
    swap(i, count-1)
    count -= 1
    indexes.remove(element)
    percolateUp(i)
    percolateDown(i)
  }

  override def update(element: A, newElement: A)
  {
    val i = indexes(element)
    indexes.remove(element)
    indexes += newElement->i
    elements(i) = newElement
    if(newElement < element)
      percolateUp(i)
    else
      percolateDown(i)
  }
}

object Heap{
  def apply[A](elementsArray: Array[A])(implicit manifest: Manifest[A], converter: A=>Ordered[A]): Heap[A] = {
    val size = elementsArray.length
    val heap = new Heap[A](size)
    heap.count = size
    for(i <- 0 until size){
      heap.elements(i) = elementsArray(i)
    }
    for(i <- ((size-1)/2) to 0 by -1)
      heap.percolateDown(i)
    heap
  }
}


object IndexedHeap{
  def apply[A](heap: Heap[A])(implicit manifest: Manifest[A], converter: A=>Ordered[A]): IndexedHeap[A] = {
    val indexedHeap = new IndexedHeap(heap.size)
    indexedHeap.elements = heap.elements
    indexedHeap.count = heap.count
    for(i <- 0 until heap.count)
      indexedHeap.indexes += heap.elements(i)->i
    indexedHeap
  }

  def apply[A](elementsArray: Array[A])(implicit manifest: Manifest[A], converter: A=>Ordered[A]): IndexedHeap[A] = {
    val heap = Heap[A](elementsArray)
    IndexedHeap(heap)
  }
}
