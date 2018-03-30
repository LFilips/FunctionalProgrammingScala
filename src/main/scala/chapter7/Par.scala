package chapter7

import java.util.concurrent.{ExecutorService, Future}

case class Par[A](value: A)

object Par {

  /**
    * Unit promotes a constants value to a parallel computation
    *
    * @param a the computation to execute
    * @tparam A the typeof the result of the computation
    * @return the result wrapped in a Par object
    */
  def unit[A](a: A): Par[A] = {
    Par(a) //the argument is evaluated here
  }

  /**
    * marks a computation for concurrent evaluation.
    * The evaluation won’t actually occur until forced by run
    */
  def fork[A](value: Par[A]) : Par[A] = ???

  /**
    * wraps its unevaluated argument in a Par and marks it for concurrent evaluation
    */
  def lazyUnit[A](a: => A): Par[A] = {
    fork(unit(a)) //the argument is evaluated here
  }

  /**
    * Get the result of the computation
    *
    * @param s the executor to use for computing A
    * @param a the par to execute
    * @tparam A the type of the result
    * @return the result
    */
    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = ???

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) {
      Par.unit(ints.headOption getOrElse 0)
    }
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }

  /**
    * Combines the results of two parallel computations with a binary function
    */
  def map2[A, B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C] = {
    Par(f(a.value, b.value))
  }

}

/**
  * N.B. section
  * While designing the bahaviour of unit or get, there is the need of choosing if doing the actually computation during
  * the get method (so this mean that we will execute the computation only when it is really needed) or immediately
  * spawn a thread and then use the get for getting the result.
  * Is not so easy to see that we need to immediatly execute the computation instead of waiting for the get otherwise
  * we will not be referential transparent because:
  *
  * val sumL = Par.unit(sum(3))
  * val sumR = Par.unit(sum(2))
  * Par.get(sumL) + Par.get(sumR)
  *
  * If we substitute with the method notation:
  * Par.get(Par.unit(sum(l))) + Par.get(Par.unit(sum(r)))
  *
  * we will have a sequential computation, because each get will wait for the result!
  *
  * The idea is to wait to call for the get until it is not necessary, so the idea is to
  * concatenate e combine computation and wait only in the end.
  *
  */

