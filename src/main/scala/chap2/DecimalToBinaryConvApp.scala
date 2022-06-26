package chap2

object DecimalToBinaryConvApp {
    def main(args: Array[String]): Unit = {
      println(decToBinConv(5))
      println(decToBinConv(8))
    }

    def decToBinConv(x: Int): String = {
      val seqOfDivByTwo = Iterator.iterate(x)(a => a / 2)
      val binList = seqOfDivByTwo.takeWhile(a => a > 0).map(a => a % 2)
      binList.mkString.reverse
    }
}
