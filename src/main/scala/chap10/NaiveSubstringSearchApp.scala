package chap10

object NaiveSubstringSearchApp {
  def main(args: Array[String]): Unit = {
    val myData1 = "This is a functional implementation."
    val myWords1 = "functional"
    println(naiveSubstringSearch(myWords1, myData1))
  }

  def naiveSubstringSearch(myWords: String, myData: String): Int = {
    myData.indices.find {
      i => i + myWords.length <= myData.length &&
        myWords.indices.forall(j => myData(j + i) == myWords(j))
    }.getOrElse(-1)
  }
}
