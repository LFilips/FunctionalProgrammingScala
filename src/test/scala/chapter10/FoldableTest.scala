package chapter10

import org.scalatest.{FlatSpec, Matchers}

class FoldableTest extends FlatSpec with Matchers{

  it should "have a foldable instance for list " in {
    foldableList.foldLeft[Int,Int](List(1,2,3,4,5))(0)(_+_) should be (15)
  }
}
