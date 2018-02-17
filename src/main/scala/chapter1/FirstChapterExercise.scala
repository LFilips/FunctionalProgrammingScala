package chapter1

object FirstChapterExercise {

  def fib(n: Int): Int = {
    require(n >= 0)
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

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  @annotation.tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = as match {
    case Array() => true
    case Array(_) => true
    case Array(first, second, _*) => ordered(first, second) && isSorted(as.drop(1), ordered)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {

    //I have to convert the function f from having two paramters to having only one

    (a) => ((b) => f(a, b))

  }

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {

    (a, b) => (f(a)(b))
  }

  /**
    *
    * Applies the f function to the result of the g function
    *
    * @param f the last function to apply
    * @param g the first function to apply
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a) => f(g(a))
  }


}
