package others

import org.scalatest.{FlatSpec, Matchers}

class InterleaveTest extends FlatSpec with Matchers {

  behavior of "InterleaveTest"

  it should "interleave 2 Strings" in {

    val a = "first"
    val b = "second"

    Interleave.interleave(a,b) should be("fsiercsotnd")

  }

  it should "give back the second string when the first is empty " in {

    val a = ""
    val b = "second"

    Interleave.interleave(a,b) should be("second")

  }

  it should "give back the second string when the second is empty" in {

    val a = ""
    val b = "second"

    Interleave.interleave(a,b) should be("second")

  }

  it should "give empty string when boht are empties" in {

    val a = ""
    val b = ""

    Interleave.interleave(a,b) should be("")

  }

}
