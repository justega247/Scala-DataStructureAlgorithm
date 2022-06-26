package chap9

object SelectionSortAscApp {
  def main(args: Array[String]): Unit = {
    println(selectionSortAsc(List(5,3,2,8,7)))
    println(selectionSortAsc(List("k","d","c","a","q")))
  }

  def selectionSortAsc[T](myData: List[T])(implicit tToOrdered: T => Ordered[T]): List[T] = myData match {
    case Nil => Nil
    case head :: Nil => List(head)
    case head :: tail =>
      val minElem = tail.min
      val indexOfMinElem = tail.indexOf(minElem)
      if (head <= minElem) {
        head :: selectionSortAsc(tail)
      } else {
        val (tailHalf1, tailHalf2) = tail.splitAt(indexOfMinElem)
        tailHalf2.head :: selectionSortAsc(tailHalf1 ::: head :: tailHalf2.tail)
      }
  }
}
