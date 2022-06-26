package chap2

case class Selection(perimeter: Int, gardenNets: List[Int])

object GardenNettingGreedyAlgoApp {
  def main(args: Array[String]): Unit = {
    val gardenNetLengths = Array(15, 10, 3, 2, 1)
    val perimeter = 50
    println(selectNets(perimeter, gardenNetLengths))
  }

  def selectNets(perimeter: Int, netLengths: Array[Int]): List[Int] = {
    val finalSelection = netLengths.foldLeft(Selection(perimeter, List())) {
      (selection, length) => {
        println(selection.gardenNets, length)
        val numbers = selection.perimeter / length
        val netsToBuy = List.fill(numbers)(length)
        Selection(selection.perimeter - numbers * length, selection.gardenNets ::: netsToBuy)
      }
    }
    finalSelection.gardenNets
  }
}
