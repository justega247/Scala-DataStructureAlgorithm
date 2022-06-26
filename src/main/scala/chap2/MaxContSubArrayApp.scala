package chap2

object MaxContSubArrayApp {
  def main(args: Array[String]): Unit = {
    val stockPriceDiff = Vector(1, -2, 5, 6, -1, 4, 9, -3, 2, 5)
    println(findContSubArrayMax(stockPriceDiff))
  }

  def findContSubArrayMax(data: Vector[Int]): Int = {
    data match {
      case Vector(x) => x
      case _ =>
        val (l, r) = data.splitAt(data.length / 2)
        val leftMax = findContSubArrayMax(l)
        val rightMax = findContSubArrayMax(r)
        val leftRightCrossMaxVal = leftRightCrossMax(l, r)
        List(leftMax, rightMax, leftRightCrossMaxVal).max
    }
  }

  def leftRightCrossMax(leftSub: Vector[Int], rightSub: Vector[Int]): Int = {
    val collLeftSums = for (i <- 1 to leftSub.length) yield leftSub.takeRight(i).sum
    val collRightSums = for (i <- 1 to rightSub.length) yield rightSub.take(i).sum
    collLeftSums.max + collRightSums.max
  }
}
