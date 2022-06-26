package chap6

class MyQueue(maxSize: Int) {
  private val queueBox: Array[Any] = new Array[Any](maxSize)
  private var front: Int = 0
  private var rear: Int = -1
  private var numOfItems: Int = 0

  def insert(data: Any): Unit = {
    if (rear == maxSize - 1) {
      rear = -1
    }

    rear += 1
    queueBox(rear) = data
    numOfItems += 1
  }

  def remove(): Any = {
    val tempData: Any = queueBox(front)
    front += 1

    if(front == maxSize) {
      front = 0
    }

    numOfItems -= 1
    tempData
  }

  def peekFront(): Any = {
    queueBox(front)
  }

  def isEmpty: Boolean = {
    numOfItems == 0
  }

  def isFull: Boolean ={
    numOfItems == maxSize
  }
}

object QueueApp {
  def main(args: Array[String]): Unit = {
    val myQueue = new MyQueue(10)

    myQueue.insert(5)
    myQueue.insert(10)
    myQueue.insert(15)

    while(!myQueue.isEmpty) {
      println(myQueue.remove())
    }
  }
}
