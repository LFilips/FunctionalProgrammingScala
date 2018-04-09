package chapter7

import java.util.concurrent.{ExecutorService, _}

import scala.language.implicitConversions
import chapter7.Par.Par


/**
  * This is a simple implementation of the future that wraps a constant value
  */
private case class UnitFuture[A](get: A) extends Future[A] {
  override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

  override def isCancelled: Boolean = false

  override def isDone: Boolean = true

  override def get(timeout: Long, unit: TimeUnit): A = get

  def getWithElapsed(timeout: Long, unit: TimeUnit): (A, Long) = {
    val start = System.currentTimeMillis()
    (get(timeout, unit), System.currentTimeMillis() - start)
    //param are always evaluated left to right so is safe to write in this way
  }
}

/**
  * Future implementation that is aware of the time already elapsed, I'll use it for chaining operation
  * being aware of the time already elapsed evaluating other task.
  * Since I need a concrete impl
  *
  * @tparam A
  */
private case class TimeAwareMap2Future[A, B, C](a: Future[A], b: Future[B])(f: (A, B) => C) extends Future[C] {

  override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

  override def isCancelled: Boolean = false

  override def isDone: Boolean = true

  override def get(timeout: Long, unit: TimeUnit): C = {

    val timeoutMillis = unit.toMillis(timeout)
    //println(s"Milliseconds total timeout: $timeoutMillis")

    val start = System.currentTimeMillis()
    val parResA = a.get(timeoutMillis, TimeUnit.MILLISECONDS)
    val elapsed = System.currentTimeMillis() - start

    //println(s"Millis elapsed evaluating A: $elapsed")
    val remainingTime = timeoutMillis - elapsed
    //println(s"Milliss remained for evaluating B: $remainingTime")

    //I'm forcing to millisecond, so there can be a loss of precision
    val startB = System.currentTimeMillis()
    val parResB = b.get(remainingTime, TimeUnit.MILLISECONDS)
    val elapsedB = System.currentTimeMillis() - startB
    //println(s"Millis elapsed evaluating B: $elapsedB")

    f(parResA, parResB)
  }

  override def get(): C = f(a.get(), b.get())
}

class ParOps[A](p: Par[A]) {

  /**
    * Wrap the Par.map in this class for calling though infix operator
    */
  def map2[B, C](b: => Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)
}


object Par {

  /**
    * This implicit definition is used for implicitly convert to another type.
    * Is nothign more than a casting method, but since it is defined with
    * the implicit it will be invoked anytime there is need of using it
    * and doesn't need to be explicitly called. However is a dangerous operation
    */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  type Par[A] = (ExecutorService) => Future[A]


  /**
    * Unit is the combinator for this type, it creates a Par[A] from a A value, and lift the value representation to a
    * function, but it does not use ane executor at all!! It just give a UnitFuture ignoring the executor and then
    * the get gives the value contained in it!
    *
    * @param a the computation to execute
    * @tparam A the typeof the result of the computation
    * @return the result wrapped in a Par object
    */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  /**
    * Marks a computation for concurrent evaluation. it is used in the derived combinator lazyUnite to have a parallel
    * excecution. The evaluation won’t actually occur until forced by run
    */
  def fork[A](a: => Par[A]): Par[A] = { (es: ExecutorService) =>
    es.submit(new Callable[A] {
      def call = a(es).get
    })
  }


  /**
    * Wraps its unevaluated argument in a Par and marks it for concurrent evaluation. It is a derived combinator
    * for the datatype, because is using the Unit combinator inside. The lazy Unit is used to proved to the programmer
    * the choice to execute the computation on another thread
    */
  def lazyUnit[A](a: => A): Par[A] = {
    fork(unit(a)) //the argument is evaluated here
  }

  /**
    * Get the result of the computation
    *
    * @param es the executor to use for computing A
    * @param a  the par to execute
    * @tparam A the type of the result
    * @return the result
    */
  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  /**
    * Combines the results of two parallel computations with a binary function
    */
  def map2[A, B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get(), bf.get))
    }
  }

  /**
    * Hard: Fix the implementation of map2 so that it respects the contract of timeouts on Future.
    */
  def map2Timeout[A, B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      TimeAwareMap2Future(af, bf)(f)
    }
  }

  /**
    * Exercise 7.4
    * This API already enables a rich set of operations. Here’s a simple example: using lazyUnit,
    * write a function to convert any function A => B to one that evaluates its result asynchronously.
    */
  def asyncF[A, B](f: A => B): A => Par[B] = {
    (a) => lazyUnit(f(a))
  }


  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  /**
    * Exercise 7.5
    * Hard: Write this function, called sequence. No additional primitives are required. Do not call run.
    *
    * The idea of the sequence is that converrt a list of par in a Par with a list A, where A in the computation
    * that need to be done. In this way we can use this primitie for implementing parMap()
    *
    */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    /**
      * The return type of the whole function should be: (ExecutorService) => Future[A]
      * in my case I have a list, so need to be (ExecutorService) => Future[List[A]
      */
    (es: ExecutorService) => {
      //need to return a Future[List[A]], i have from the inout a List[Par[A]]
      //map2(ps.head,ps.head)((a,b) => List(a,b))(es) this should be the two element combining function
     //val mixing2Element : Future[List[A]] = map2(ps.head,ps.head)((a2,b2) => List(a2,b2))(es)

      ps.foldRight(unit(List()) : Par[List[A]])((elemA,elemB) => map2(elemA,elemB)((a2,b2) => a2 :: b2))(es)

    }

  }


  def sequenceShort[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight(unit(List()) : Par[List[A]])((elemA,elemB) => map2(elemA,elemB)((a2,b2) => a2 :: b2))

  /**
    * The idea of parMap function is that you can do the map operation in parallel for a list.
    * Is easy to imagine having a list of par, so we have a list[(executor) => (Future[A]), beacuase we can simply
    * use the async function that we already have to
    * @return
    */
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = {
    /**
      * val fbs: List Par B = ps.map(asyncF(f)) This is the signature on the book
      * but is a bit bad because is hard to understant how the async will produce the par
      */
    val fbs: List[Par[B]] = ps.map((a) => asyncF(f)(a))
    sequence(fbs)
  }


  /**
    * Write a function which filter the list in parallel. //TODO
    */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = ???

}


/**
  * The main idea is to create an api that will allow to do parallel computation. Let's take as example the sum
  * of all the integer of a list, this can be easily achieved using the fold right function.
  * But what if we want to do this with many threads? how we can do that? How that can be done in a pure functional approach?
  * Instaed of using the fold function we can use a recursive approach, where we split the list in two and then we sum the
  * result again:
  * val (l,r) = list.splitAt(length/2)
  * sum(l) + sum(r)
  *
  * For doing this, we need to wrap this sum operation in out parallel operation, that can be for example Par[A], that
  * holding a generic type can hold everything.
  * The first definition is about a Unit and a get. The unit will be the creation operator for our data type, and get
  * will be the method that we use for getting data from it. The only problem is that we have to decide where the
  * actual calculation of the data will occurr, if directly in the unit or when the get is called.
  *
  * This is an example of how it can be implemented:
  *
  * * val sumL = Par.unit(sum(3))
  * * val sumR = Par.unit(sum(2))
  * * Par.get(sumL) + Par.get(sumR)
  *
  * In this case we have two different solution:
  * 1) starting evaluating the sum when the unit is created
  * 2) Delay the evaluation of the param until get is called
  *
  * If we want any degree of parallelism we need unit to begin to evaluate the value immediatly otherwise since the
  * function argument are evaluated left to right we will run sumR always after sumL, since we will block for the get
  * to have some value. But what happen if we start to evaluate everything immediatly in the Unit? That we break the
  * referential transparency, because if we substitute with the method notation:
  *
  * Par.get(Par.unit(sum(l))) + Par.get(Par.unit(sum(r)))
  *
  * we will have a sequential computation, because each get will wait for the result!
  * The referential transparency is broken only with regard of get, so until we are not calling it we are not in trouble.
  *
  * So the assumption is that the unit will not longer do the computation but will be the get to take care of that, the unit
  * will be used to create a unit of computation, that we will be able to combine and manipulate.
  *
  *
  *
  *
  *
  */



