package chap5

class MyStack(maxSize: Int) {
  private val stackBox = new Array[Double](maxSize)
  private var top = -1

  def push(data: Double): Unit = {
    top += 1
    stackBox(top) = data
  }

  def pop(): Double = {
    val popData = stackBox(top)
    top -= 1
    popData
  }

  def peek(): Double = {
    stackBox(top)
  }

  def isEmpty: Boolean = {
    top == -1
  }

  def isFull: Boolean = {
    top == maxSize - 1
  }
}

object StackApp {
  def main(args: Array[String]): Unit = {
    val myStack = new MyStack(8)
    myStack.push(5)
    myStack.push(10)
    myStack.push(20)

    while(!myStack.isEmpty) {
      println(myStack.pop())
    }
  }
}
