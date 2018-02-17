import scala.collection.mutable

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
  case Array(first, second, _*) => s"$first $second"

}

arrayPatternMatching(Array())
arrayPatternMatching(Array(1, 2, 3))


def fib(n: Int): Int = {


  @annotation.tailrec
  def tailFib(actual: Int, index: Int, prev: Int, prev2: Int): Int = {
    if (actual == index) return prev + prev2
    else tailFib(actual + 1, index, prev + prev2, prev)

  }

  n match {
    case 0 => return 0
    case 1 => return 1
    case _ => tailFib(2, n, 1, 0)
  }

}

fib(10)



def f(num: Int, arr: List[Int]): List[Int] = {

  var list: List[Int] = List() //create an empty list

  for (i <- arr.indices) {
    list = list ::: multiplyElem(arr(i), num)
  }

  /**
    * Takes an element and the number of times it need to be multiplied
    **/
  def multiplyElem(elemToMultiply: Int, num: Int): List[Int] = {

    var list: List[Int] = List() //create an empty list

    for (i <- 1 to num) {
      list = list ::: List(elemToMultiply)
    }

    list

  }

  list

}


/* Curring example */


def filterValue(value: Int)(limit: Int) = value <= limit

val curriedFilter = filterValue(3)(_)

List(1, 2, 3, 4).filter(curriedFilter)


def ex(delim: Int, arr: List[Int]): List[Int] = {

  def filterValue(limit: Int)(value: Int) = value <= limit

  val curriedFilter = filterValue(delim)(_)

  arr.filter(elem => curriedFilter(elem))

}


ex(2, List(1, 2, 3, 4))





def removeElemOddPosition(arr: List[Int]): List[Int] = {
  /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/

  arr.indices.collect {
    case i if i % 2 == 1 => arr(i)
  }.toList

}


def emitList(num: Int): List[Int] = {

  def creatList(size: Int) = {

    val array = new Array[Int](num)

    for (i <- 0 to size - 1) {
      array.update(i, i)
    }

    array.toList
  }

  val list = creatList(num)

  print(list + "\n")
  return list
}



emitList(9)


def reverse(arr: List[Int]): List[Int] = {

  var list: List[Int] = List()

  for (i <- arr.size - 1 to 0 by -1) {
    list = list ++ List(arr(i))
  }

  list

}


reverse(List(1, 2, 3, 4))


def sumOdd(arr: List[Int]): Int = {
  arr.filter(_ % 2 == 1).fold(0)(_ + _)
}


def calculateSize(arr: List[Int]): Int = arr match {
  case _ :: tail => 1 + calculateSize(tail)
  case _ => 0
}


calculateSize(List(1, 2, 3, 4))

def absolute(arr: List[Int]): List[Int] = {
  arr.map((elem) => if (elem < 0) -(elem) else elem
  )
}


List(1, 2, 3, 4, 5, 6).zipWithIndex



def exponentialEuler(n: Double): Double = {

  def factorial(n: Int): Int = n match {
    case 0 => 1
    case n => n * factorial(n - 1)
  }

  def exponent(times: Int, value: Double): Double = (times,value) match {
    case (0,_) => 1
    case (times,value) => value * exponent(times-1,value)
  }

  var nthElementSum: Double = 0.0

  for (i <- 2 to 9){
    nthElementSum = nthElementSum + exponent(i,n)/factorial(i)
  }

  1.0 + n + nthElementSum

}




def factorial(n: Int): Int = n match {
  case 0 => 1
  case n => n * factorial(n - 1)
}

def exponent(times: Int, value: Double): Double = (times,value) match {
  case (0,_) => 1
  case (times,value) => value * exponent(times-1,value)
}



factorial(10)
exponent(3,3)

exponentialEuler(20)
