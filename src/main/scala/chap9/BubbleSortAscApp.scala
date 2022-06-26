package chap9

object BubbleSortAscApp {
  def main(args: Array[String]): Unit = {
    println(bubbleSortAsc(List(3, 1, 6, 8, 2)))
    println(bubbleSortAsc(List("z", "c", "a", "b")))
  }

  def bubbleSortAsc[T](myData: List[T])(implicit tToOrdered: T => Ordered[T]): List[T] = myData match {
    case Nil => Nil
    case _ =>
      val (max, remainingData) = calcMax(myData)
      bubbleSortAsc(remainingData) ::: List(max)
  }

  def calcMax[T](myData: List[T])(implicit tToOrdered: T => Ordered[T]): (T, List[T]) = myData match {
    case Nil => (null.asInstanceOf[T], Nil)
    case head :: Nil => (head, Nil)
    case head :: tail =>
      val (tailMax, tailRemaining) = calcMax(tail)
      if (tailMax >= head) (tailMax, head :: tailRemaining)
      else (head, tailMax :: tailRemaining)
  }
}
