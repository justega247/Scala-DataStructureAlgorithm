package chap11

trait MyGraph[V] {
  def vertices: List[V]
  def edges: List[(V,V)]
  def addEdge(a: V, b: V): MyGraph[V]
  def neighbors(x: V): List[V]
}

object MyGraph {
  def apply[V](adjacencyList: Map[V, List[V]]): MyGraph[V] =
    new MyDirectedGraph(adjacencyList)
  def apply[V](): MyGraph[V] = new MyDirectedGraph(Map[V,
    List[V]]())
}

class MyDirectedGraph[V](adjacencyList: Map[V, List[V]]) extends MyGraph[V] {
  override def vertices: List[V] = adjacencyList.keys.toList
  override def edges: List[(V, V)] = adjacencyList.flatMap {
    case (v, neighbors) => neighbors.map(x =>(v,x))
  }.toList
  override def addEdge(a: V, b: V): MyDirectedGraph[V] = {
    val aNeighbors = b +: neighbors(a)
    new MyDirectedGraph(adjacencyList + (a -> aNeighbors))
  }
  override def neighbors(x: V): List[V] =
    adjacencyList.getOrElse(x, Nil)
}

class MyUndirectedGraph[V](adjacencyList: Map[V, List[V]])
  extends MyDirectedGraph[V](adjacencyList) {
  override def addEdge(a: V, b: V): MyUndirectedGraph[V] = {
    val aNeighbors = b +: neighbors(a)
    val bNeighbors = a +: neighbors(b)
    new MyUndirectedGraph(adjacencyList + (a -> aNeighbors, b -> bNeighbors))
  }
}

case class MyWeightedEdge[V](dest: V, weight: Int)

class MyWeightedGraph[V](adjacencyList: Map[V, List[MyWeightedEdge[V]]]) extends MyGraph[V] {
  override def vertices: List[V] = adjacencyList.keys.toList

  override def edges: List[(V, V)] = adjacencyList.flatMap {
    case (v, edgeList) => edgeList.map(e => v -> e.dest)
  }.toList

  def addEdge(a: V, weightedEdge: MyWeightedEdge[V]):
    MyWeightedGraph[V] = {
      val aNeighbors = weightedEdge +:
        adjacencyList.getOrElse(a, Nil)
      new MyWeightedGraph(adjacencyList + (a -> aNeighbors))
    }

  override def addEdge(a: V, b: V): MyWeightedGraph[V] =
    addEdge(a, MyWeightedEdge(b, weight = 0))

  override def neighbors(x: V): List[V] =
    adjacencyList.getOrElse(x, Nil).map(_.dest)

  def neighborsWithWeight(x: V): List[MyWeightedEdge[V]] =
    adjacencyList.getOrElse(x, Nil)
}

object MyGraphApp {
  def main(args: Array[String]): Unit = {
    val myGraph = MyGraph[String]()
      .addEdge("San Francisco", "Hong Kong")
      .addEdge("Hong Kong", "Kathmandu")
      .addEdge("Kathmandu", "San Francisco")
      .addEdge("Kathmandu", "Bangkok")
      .addEdge("Bangkok", "San Francisco")
      .addEdge("Pokhara", "Bangkok")
    println(myGraph.vertices)
    println(myGraph.neighbors("Kathmandu"))
  }
}