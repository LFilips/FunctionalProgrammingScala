package chapter12

import org.scalatest.{FlatSpec, Matchers}

class ApplicativeTest extends FlatSpec with Matchers {

  "Validation applicative" should "map4 4 instance of applicative" in {

    val stringErrorValidationApplicative = Applicative.validationApplicative[String]

    val intStringApplicative = Success(3)
    val intStringApplicative2 = Success(4)
    val intStringApplicative3 = Success(5)
    val intStringApplicative4 = Success(6)
    val failureApplicative = Failure("error") //default defijnitio with empty vector
    val failureApplicative2 = Failure("error1", Vector("error2"))
    val failureApplicative3 = Failure("error3", Vector("error4"))

    stringErrorValidationApplicative
      .map4(intStringApplicative,
        intStringApplicative2,
        intStringApplicative3,
        intStringApplicative4)((a, b, c, d) => a + b + c + d) should be(Success(18))

    /** Only one error */
    stringErrorValidationApplicative
      .map4(intStringApplicative,
        intStringApplicative2,
        failureApplicative,
        intStringApplicative4)((a, b, c, d) => a + b + c + d) should be(Failure("error"))

    /** Many Error */
    stringErrorValidationApplicative
      .map4(intStringApplicative,
        failureApplicative3,
        failureApplicative,
        failureApplicative2)((a, b, c, d) => a + b + c + d) should be(Failure("error1",Vector("error", "error2", "error3", "error4")))



  }

}
