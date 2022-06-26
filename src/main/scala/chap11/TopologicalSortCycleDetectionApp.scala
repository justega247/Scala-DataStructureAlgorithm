package chap11

object TopologicalSortCycleDetectionApp {
  def main(args: Array[String]): Unit = {
    val myEventsList = List(("prepare_test", "take_test"),
      ("take_test", "apply"),
      ("apply", "receive_offer"),
      ("receive_offer", "join_CMU"),
      ("join_CMU", "choose_major"),
      ("choose_major", "graduate"),
      ("receive_offer", "join_MIT"),
      ("join_MIT", "choose_major"),
      ("choose_major", "graduate"),
      ("choose_major", "join_MIT"))
    println(topoSortDetectCycle(myEventsList))
  }

  def topoSortDetectCycle(graph: List[(String, String)]): (List[String], List[String]) = {
    def topoSort(vertices: List[String], path: List[String], vc: (List[String], List[String])): (List[String], List[String]) = vertices match {
      case Nil => vc
      case x :: xs =>
        val (visited, cycle) = vc
        topoSort(xs, path,
          if (path.contains(x)) (visited, x :: cycle)
          else if (visited.contains(x)) vc
          else addToVisitedTL(x, topoSort(
            calcSuccessorSet(x, graph), x :: path, vc))
        )
    }
    val (start, _) = graph.unzip
    val result = topoSort(start, List(), (List(), List()))
    result
  }

  def calcSuccessorSet(vertex: String, graph: List[(String,
    String)]): List[String] = graph match {
    case Nil => Nil
    case x :: xs if vertex == x._1 => x._2 ::
      calcSuccessorSet(vertex, xs)
    case _ :: xs => calcSuccessorSet(vertex, xs)
  }

  def addToVisitedTL(value: String, vc: (List[String],
    List[String])): (List[String], List[String]) = (value :: vc._1, vc._2)
}
