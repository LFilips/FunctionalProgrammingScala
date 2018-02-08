

/**
  *
  * Worksheet for playing a bit with scala
  *
  */

//this will help understand how patter matching works

object GivenNames {
  def unapplySeq(name: String): Option[Seq[String]] = {
    val names = name.trim.split(" ")
    if (names.forall(_.isEmpty)) None else Some(names)
  }
}

def greetWithFirstName(name: String) = name match {
  case GivenNames(firstName, _*) => "Good morning, " + firstName + "!"
  case _ => "Welcome! Please make sure to fill in your name!"
}



def applyPatternMatch(x: List[String]) = x match {
  case List("ciao") => true
  case _ => false
}



List.apply(1, 3, 4)
List.unapplySeq(List(2, 3, 4))



applyPatternMatch(List("ciao"))



//testing array pattern matching


def arrayPatternMatching(ar: Array[Int]): String = ar match {

  case Array() => "empty"
  case Array(first, second,_*) => s"$first $second"

}

arrayPatternMatching(Array())
arrayPatternMatching(Array(1, 2, 3))