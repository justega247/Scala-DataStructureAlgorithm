package chap9

object InsertionSortAscApp {
  def main(args: Array[String]): Unit = {
    println(insertionSortAsc(List(15, 10, 33, 11)))
    println(insertionSortAsc(List("banana", "apple", "mango", "cashew")))
  }

  def insertionSortAsc[T](myData: List[T])(implicit tToOrdered: T => Ordered[T]): List[T] = myData match {
    case Nil => myData
    case head :: tail =>
      val temp = insertionSortAsc(tail)
      insertElementAsc(head, temp)
  }

  def insertElementAsc[T](elem: T, sortedSubList: List[T])(implicit tToOrdered: T => Ordered[T]): List[T] = sortedSubList match {
    case Nil => elem :: sortedSubList
    case head :: tail =>
      if (head <= elem) {
        head :: insertElementAsc(elem, tail)
      } else {
        elem :: sortedSubList
      }
  }
}
