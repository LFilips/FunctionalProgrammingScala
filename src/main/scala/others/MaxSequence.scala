package others

import org.scalatest.{FlatSpec, Matchers}

sealed trait Tree[+A] {
  val value: A
}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {


  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(_, left, right) => 1 + depth(left).max(depth(right))
  }

  def maxSequence(tree: Tree[Int]): Int = tree match {
    case Leaf(_) => 1
    case Branch(value, left, right) => if ((value + 1 == left.value) && (value + 1 == right.value + 1)) 1 + maxSequence(left).max(maxSequence(right))
                                        else
                                            if (value + 1 == left.value) 1 + maxSequence(left)
                                            else
                                                if (value + 1 == right.value) 1 + maxSequence(right)
                                                else 1
  }
}

class TreeTest extends FlatSpec with Matchers {

  "The sequence method" should "find the longest sequence" in {

    val tree = Branch(1,
      Branch(2, Leaf(3), Leaf(4)), Branch(5, Leaf(7), Leaf(4)))

    Tree.maxSequence(tree) should be(3)

  }

  it should "find the longest sequence 2" in {

    val tree = Branch(1,
      Branch(0, Leaf(8), Leaf(0)), Branch(0, Leaf(6), Leaf(0)))

    Tree.maxSequence(tree) should be(1)

  }

  it should "find the longest sequence 3" in {

    val tree = Branch(1,
      Branch(2, Branch(3,Leaf(8),Leaf(4)), Leaf(0)), Branch(0, Leaf(6), Leaf(0)))

    Tree.maxSequence(tree) should be(4)

  }

  List(1,2,3).fold()

}
