package chapter5

import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class StrictnessLazynessTest extends FlatSpec with Matchers with MockFactory{

  ignore should "lazily produce the new value" in {

    val mockedTrueFunction = mockFunction[String]
    val mockedFalseFunction = mockFunction[String]

    mockedTrueFunction.expects().never()

    StrictnessLazyness.if2(false, mockedTrueFunction(), mockedFalseFunction())

  }

}
