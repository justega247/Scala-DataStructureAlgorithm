package chap10

object KnuthMorrisPrattApp {
  def main(args: Array[String]): Unit = {
    val myData1 = "This is a functional implementation."
    val myWords1 = "functional"
    println(kmpSubstringSearch(myWords1, myData1))
  }

  def kmpSubstringSearch(myWords: String, myData: String): Int = {
    val prefixTab = prefixTable(myWords)

    myData.indices.foldLeft(-1, 0) {
      case ((foundIndex, x), i) if foundIndex > 0 => (foundIndex, 0)
      case ((foundIndex, x), i) =>
        val stepsX = LazyList.iterate(x)(x => prefixTab(x - 1))
        val lowerX = stepsX.find(x => x == 0 || myWords(x) == myData(i)).get
        val newX = if (myWords(lowerX) == myData(i))
          lowerX + 1 else lowerX
        if (newX == myWords.length) (i - newX + 1, 0) else (-1, newX)
    }._1
  }

  def prefixTable(searchString: String): Vector[Int] = {
    searchString.drop(1).foldLeft(0, Vector(0)) {
      case ((initialValue, prefixT), currentCharacter) =>
        val lowerValue = LazyList.iterate(initialValue)(
          initialValue => prefixT(initialValue - 1)
        ).find(initialValue => initialValue == 0 || searchString(initialValue) == currentCharacter).get
        val newValue =  if (searchString(lowerValue) == currentCharacter) lowerValue + 1 else lowerValue

        (newValue, prefixT :+ newValue)
    }._2
  }
}
