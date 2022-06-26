package chap8

object BinTreeFlipCheckApp {
  def main(args: Array[String]): Unit = {
    val myList = List(1,2,3,4,5,6)
    val myBinTree = createTree(myList)
    val myBinTreeFlipped = flip(myBinTree)
    println(myBinTree)
    println(myBinTreeFlipped)
    println(flipEqual(myBinTree, myBinTreeFlipped))
  }

  def flipEqual(binTree1: BinaryTree[Int], binTree2: BinaryTree[Int]): Boolean = (binTree1, binTree2) match {
    case (Leaf, Leaf) => true
    case (Branch(value1, leftBranch1, rightBranch1), Branch(value2, leftBranch2, rightBranch2)) if
      value1 == value2 => flipEqual(leftBranch1, rightBranch2) && flipEqual(leftBranch2, rightBranch1)
    case _ => false
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
