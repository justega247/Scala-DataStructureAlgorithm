package chap5

class MyStackChar(maxSize: Int) {
  private val stackBox = new Array[Char](maxSize)
  private var top = -1

  def push(data: Char): Unit = {
    top += 1
    stackBox(top) = data
  }

  def pop: Char = {
    val popData = stackBox(top)
    top -= 1
    popData
  }

  def peek: Char = {
    stackBox(top)
  }

  def isEmpty: Boolean = {
    top == -1
  }

  def isFull: Boolean = {
    top == maxSize - 1
  }
}

class Reverser(word: String) {
  private val output: StringBuilder = new StringBuilder

  def reverse: StringBuilder = {
    val myStack = new MyStackChar(word.length)

    for(eachChar <- word) {
      myStack.push(eachChar)
    }

    while(!myStack.isEmpty) {
      val poppedChar: Char = myStack.pop
      output.append(poppedChar)
    }
    output
  }
}

object WordReverseApp {
  def main(args: Array[String]): Unit = {
    print("Enter a word: ")
    val inputWord = scala.io.StdIn.readLine()
    val myReverser = new Reverser(inputWord)

    println("Reverse word: " + myReverser.reverse)
  }
}
