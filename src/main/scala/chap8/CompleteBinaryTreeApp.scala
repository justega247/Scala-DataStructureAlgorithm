package chap8

object CompleteBinaryTreeApp {
  def main(args: Array[String]): Unit = {
    val myCompleteBinTree = createCompleteBinaryTree(2, 3)
    println(myCompleteBinTree)
  }

  def createCompleteBinaryTree(value: Int, depth: Int): BinaryTree[Int] =
    if (depth == 0) Leaf
    else Branch(
      value,
      createCompleteBinaryTree(2 * value, depth -1),
      createCompleteBinaryTree(2 * value + 1, depth - 1)
    )
}
