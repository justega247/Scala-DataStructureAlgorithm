package chap11

object GraphApp {
  def main(args: Array[String]): Unit = {
    val myGraph = List(("a", "b"), ("a", "c"), ("b", "d"), ("b", "e"), ("b", "c"), ("c", "e"), ("d", "f"))

    println(calcSuccessorSet("b", myGraph))
  }

  def calcSuccessorSet(vertex: String, graph: List[(String, String)]): List[String] = graph match {
    case Nil => Nil
    case x :: xs if vertex == x._1 => x._2 :: calcSuccessorSet(vertex, xs)
    case _ :: xs => calcSuccessorSet(vertex, xs)
  }
}
