package chap8

import scala.annotation.tailrec

sealed trait BinaryTree[+A]

case object Leaf extends BinaryTree[Nothing]

case class Branch[A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

object BinaryTreeApp {
  def main(args: Array[String]): Unit = {
    val myList = List(1, 2, 3, 4, 5, 6)
    val myBinTree = createTree(myList)

    println(myBinTree)
    println(size(myBinTree))
    println(depth(myBinTree))
  }

  def createTree[A](list: List[A]): BinaryTree[A] = {
    list match {
      case Nil => Leaf
      case x :: xs =>
        val halfLength = xs.length / 2
        Branch(x, createTree(xs.take(halfLength)), createTree(xs.drop(halfLength)))
    }
  }

  def size[A](binTree: BinaryTree[A]): Int = binTree match {
    case Leaf => 0
    case Branch(_, leftBranch, rightBranch) => 1 + size(leftBranch) + size(rightBranch)
  }

  def depth[A](binTree: BinaryTree[A]): Int = binTree match {
    case Leaf => 0
    case Branch(_, leftBranch, rightBranch) => 1 + (depth(leftBranch) max depth(rightBranch))
  }
}


object BinTreeTraversal {
  def main(args: Array[String]): Unit = {
    val myList = List(1, 2, 3, 4, 5, 6)
    val myBinTree = createTree(myList)

    println(myBinTree)
    println(preorder(myBinTree))
    println(inorder(myBinTree))
    println(postorder(myBinTree))
  }

  def preorder[A](binTree: BinaryTree[A]): List[A] = binTree match {
    case Leaf => Nil
    case Branch(value, leftBranch, rightBranch) => value +: (preorder(leftBranch) ++ preorder(rightBranch))
  }

  def inorder[A](binTree: BinaryTree[A]): List[A] = binTree match {
    case Leaf => Nil
    case Branch(value, leftBranch, rightBranch) => inorder(leftBranch) ++ (value +: inorder(rightBranch))
  }

  def postorder[A](binTree: BinaryTree[A]): List[A] = binTree match {
    case Leaf => Nil
    case Branch(value, leftBranch, rightBranch) => postorder(leftBranch) ++ postorder(rightBranch) ++ List(value)
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


object BinarySearchTreeApp {
  type Dictionary[A] = BinaryTree[(String, A)]

  def empty[A](): Dictionary[A] = Leaf

  def main(args: Array[String]): Unit = {
    val myWordList = List(("cat", 5), ("dog", 7), ("the", 12), ("for", 4), ("then", 11))

    val myBinSearchTree = myWordList.foldLeft(empty[Int]())((y, x) => insert(x._1, x._2, y))

    println(inorder(myBinSearchTree))
    println(preorder(myBinSearchTree))
    println(postorder(myBinSearchTree))
    println(searchKey("for", myBinSearchTree))
  }

  def insert[A](key: String, value: A, dict: Dictionary[A]): Dictionary[A] = dict match {
    case Leaf => Branch((key, value), Leaf, Leaf)
    case Branch((k, _), _, _) if key == k => sys.error(s"key $key already present")
    case Branch((k, v), lb, rb) if key < k => Branch((k, v), insert(key, value, lb), rb)
    case Branch((k, v), lb, rb) if key > k => Branch((k, v), lb, insert(key, value, rb))
  }

  @tailrec
  def searchKey[A](key: String, dict: Dictionary[A]): Option[A] = dict match {
    case Leaf => None
    case Branch((k, v), _, _) if key == k => Some(v)
    case Branch((k, _), lb, _) if key < k => searchKey(key, lb)
    case Branch((k, _), _, rb) if key > k => searchKey(key, rb)
  }

  def updateValue[A](key: String, value: A, dict: Dictionary[A]): Dictionary[A] = dict match {
    case Leaf => Branch((key, value), Leaf, Leaf)
    case Branch((k, _), lb, rb) if key == k => Branch((k, value), lb, rb)
    case Branch((k, _), lb, rb) if key < k => Branch((k, value), updateValue(key, value, lb), rb)
    case Branch((k, _), lb, rb) if key > k => Branch((k, value), lb, updateValue(key, value, rb))
  }

  def preorder[A](binTree: BinaryTree[A]): List[A] = binTree match {
    case Leaf => Nil
    case Branch(value, leftBranch, rightBranch) => value +: (preorder(leftBranch) ++ preorder(rightBranch))
  }

  def inorder[A](binTree: BinaryTree[A]): List[A] = binTree match {
    case Leaf => Nil
    case Branch(value, leftBranch, rightBranch) => inorder(leftBranch) ++ (value +: inorder(rightBranch))
  }

  def postorder[A](binTree: BinaryTree[A]): List[A] = binTree match {
    case Leaf => Nil
    case Branch(value, leftBranch, rightBranch) => postorder(leftBranch) ++ postorder(rightBranch) ++ List(value)
  }
}