package util

import collection.immutable.List._

abstract class Tree[+A] {
  def map[B](f: A => B): Tree[B]
  def flatMap[B](f: A=>Tree[B]): Tree[B]
  def foreach(f: A=>Unit)
  def filter(predicate: A => Boolean): Tree[A]
}

case object EmptyTree extends Tree[Nothing]
{
  def map[B](f: Nothing => B) = EmptyTree
  def flatMap[B](f: Nothing => Tree[B]) = EmptyTree
  def filter(predicate: Nothing => Boolean) = EmptyTree
  def foreach(f: Nothing=>Unit) {}
  override def toString = "EmptyTree"
}

case class Node[A] (value: A, var children: List[Tree[A]]) extends Tree[A] {
  
  override def map[B](f: A=>B): Tree[B] = Node[B](f(value), children.map(_.map(f)))

  def addChildren(childrenToAdd: Iterable[Tree[A]]) {
    children = {for(Node(v, c) <- childrenToAdd)
      yield Node(v, c)}.toList ::: children
  }

  def addChildrenValues(childrenValues: Iterable[A]) {
    addChildren(childrenValues.map(Node(_)))
  }

  def flatMap[B] (f:A=>Tree[B]): Tree[B] = f(value) match {
    case EmptyTree => EmptyTree
    case Node(newValue, newChildren) =>
      Node(
        newValue,
        {for(Node(v, c) <- children.map(_.flatMap(f)))
          yield Node(v, c)}.toList)
  }

  def foreach(f: A=>Unit) {
    f(value)
    children.foreach(_.foreach(f))
  }

  def filter(predicate: A => Boolean): Tree[A] =
    this.flatMap[A](v=>if(predicate(v)) Tree(v) else EmptyTree)

  override def toString: String = {
    "Node(" + value.toString + children.foldLeft(", [")(_ + _.toString + " ") + "])"
  }
  
}

object Tree {
  def apply[A](value: A): Tree[A] = Node(value)
}

object Node {
  def apply[A](value: A): Node[A] = new Node(value, List[Tree[A]]())
}