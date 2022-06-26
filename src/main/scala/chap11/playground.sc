// Getting the value for a given integer index

val myNumList: List[Int] = List(2, 4, 6, 8, 9)


def get[A](list: List[A], index: Int): A =
  list.tail.foldLeft(list.head, 0) {
    (x, y) => {
      if (x._2 == index) x else (y, x._2 + 1)
    }
  } match {
    case (result, ind) if index == ind => result
    case _ => throw new Exception("Incorrect index")
  }

get(myNumList, 0)
get(myNumList, 2)
get(myNumList, 3)

// Calculating the average of values
val myNewNumList = List(2.0, 4.0, 6.0)

def calcAverage(list: List[Double]): Double = list match {
  case head :: tail => tail.foldLeft(head, 1.0) {
    (x, y) =>
      ((x._1 * x._2 + y) / (x._2 + 1.0), x._2 + 1.0)
  }._1
  case Nil => throw new Exception("Not a number.")
}

calcAverage(myNewNumList)