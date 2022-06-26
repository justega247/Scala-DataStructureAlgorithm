package chap11

import scala.annotation.tailrec

object GraphTraversalApp {
  def main(args: Array[String]): Unit = {
    val myGraph = List(("a", "b"), ("a", "c"), ("b", "d"), ("b", "e"), ("b", "c"), ("c", "e"), ("d", "f"))

    println(traverseDepthFirst("a", myGraph))
    println(traverseBreadthFirst("a", myGraph))
  }

  def traverseDepthFirst(start: String, graph: List[(String, String)]): List[String] = {
    def depthFirst(vertices: List[String], visited: List[String]): List[String] = vertices match {
      case Nil => visited
      case x :: xs => depthFirst(
        xs,
        if (visited.contains(x)) visited
        else depthFirst(calcSuccessorSet(x, graph), x :: visited)
      )
    }

    val result = depthFirst(List(start), List())
    result.reverse
  }

  def traverseBreadthFirst(start: String, graph: List[(String, String)]): List[String] = {
    @tailrec
    def breadthFirst(vertices: List[String], visited: List[String]): List[String] = vertices match {
      case Nil => visited
      case x :: xs if visited.contains(x) => breadthFirst(xs, visited)
      case x :: xs => breadthFirst(xs ++ calcSuccessorSet(x, graph), x :: visited)
    }

    val result = breadthFirst(List(start), List())
    result.reverse
  }

  def calcSuccessorSet(vertex: String, graph: List[(String, String)]): List[String] = graph match {
    case Nil => Nil
    case x :: xs if vertex == x._1 => x._2 :: calcSuccessorSet(vertex, xs)
    case _ :: xs => calcSuccessorSet(vertex, xs)
  }
}
