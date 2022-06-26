package chap8

object CompareBinTreesApp {
  def main(args: Array[String]): Unit = {
    val binTree1 = createTree(List(1,2,3,4,5))
    val binTree2 = createTree(List(2,1,3,4,5))
    println(equal(binTree1, binTree2))
  }

  def equal[A](binTree1: BinaryTree[A], binTree2: BinaryTree[A]): Boolean = (binTree1, binTree2) match {
      case (Leaf, Leaf) => true
      case (Branch(v1, lb1, rb1), Branch(v2, lb2, rb2)) if v1 == v2 => equal(lb1, lb2) && equal(rb1, rb2)
      case _ => false
  }

  def createTree[A](list: List[A]): BinaryTree[A] = list match {
    case Nil => Leaf
    case x :: xs =>
      val halfLength = xs.length / 2
      Branch(x, createTree(xs.take(halfLength)),
        createTree(xs.drop(halfLength)))
    }
}
