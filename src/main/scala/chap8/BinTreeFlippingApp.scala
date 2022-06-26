package chap8

object BinTreeFlippingApp {
  def main(args: Array[String]): Unit = {
    val myList = List(1,2,3,4,5,6)
    val myBinTree = createTree(myList)
    println(myBinTree)
    println(flip(myBinTree))
  }

  def flip[A](binTree: BinaryTree[A]): BinaryTree[A] = binTree match {
    case Leaf => Leaf
    case Branch(value, leftBranch, rightBranch) => Branch(value, flip(rightBranch), flip(leftBranch))
  }

  def createTree[A](list: List[A]): BinaryTree[A] = {
    list match {
      case Nil => Leaf
      case x :: xs =>
        val halfLength = xs.length / 2
        Branch(x, createTree(xs.take(halfLength)), createTree(xs.drop(halfLength)))
    }
  }
}
