package chap9

object MergeSortAscApp {
  def main(args: Array[String]): Unit = {
    println(mergeSortAsc(List(5, 6, 2, 3, 1)))
    println(mergeSortAsc(List("cat", "put", "bag")))
  }

  def mergeSortAsc[T](myData: List[T])(implicit tToOrdered: T => Ordered[T]): List[T] = myData match {
    case Nil => myData
    case _ :: Nil => myData
    case _ =>
      val (myDataSplit1, myDataSplit2) = split(myData)
      val sortedSL1 = mergeSortAsc(myDataSplit1)
      val sortedSL2 = mergeSortAsc(myDataSplit2)
      mergeAsc(sortedSL1, sortedSL2)
  }

  def mergeAsc[T](sortedSubList1: List[T], sortedSubList2: List[T])(implicit tToOrdered: T => Ordered[T]): List[T] =
    (sortedSubList1, sortedSubList2) match {
      case (sortedSubList1, Nil) => sortedSubList1
      case (Nil, sortedSubList2) => sortedSubList2
      case (x1 :: y1, x2 :: y2) =>
        if(x1 > x2) x2 :: mergeAsc(sortedSubList1, y2)
        else x1 :: mergeAsc(y1, sortedSubList2)
    }

  def split[T](myData: List[T])(implicit tToOrdered: T => Ordered[T]): (List[T], List[T]) = myData match {
    case Nil => (Nil, Nil)
    case headOfmyData :: tailOfmyData =>
      if (tailOfmyData == Nil) {
        (headOfmyData :: Nil, Nil)
      } else {
        val headOfTailOfmyData = tailOfmyData.head
        val tailOfTailOfmyData = tailOfmyData.tail
        val (tailOfTailOfmyDataSplit1, tailOfTailOfmyDataSplit2) = split(tailOfTailOfmyData)
        (headOfmyData :: tailOfTailOfmyDataSplit1, headOfTailOfmyData :: tailOfTailOfmyDataSplit2)
      }
  }
}
