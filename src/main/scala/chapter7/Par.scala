package chapter7

import java.util.concurrent._

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
    val parResA = a.get(timeoutMillis,TimeUnit.MILLISECONDS)
    val elapsed = System.currentTimeMillis() - start

    //println(s"Millis elapsed evaluating A: $elapsed")
    val remainingTime = timeoutMillis - elapsed
    //println(s"Milliss remained for evaluating B: $remainingTime")

    //I'm forcing to millisecond, so there can be a loss of precision
    val startB = System.currentTimeMillis()
    val parResB = b.get(remainingTime,TimeUnit.MILLISECONDS)
    val elapsedB = System.currentTimeMillis() - startB
    //println(s"Millis elapsed evaluating B: $elapsedB")

    f(parResA, parResB)
  }

  override def get(): C = f(a.get(), b.get())
}


object Par {
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
    *
    * The fix the timeout problem i need to know before how much will be the upper bound of the timeout.
    * But I cant do that, because this is something that will be provided during the get.
    * So what I'll need to do is remember is some way the time aldreay elapsed calculating each element
    * and then subtract this to the total time that I have availabe.
    * But the time that i have to wait should be counted only from the moment when i start the get,
    * why this is connected with the map per se? Should be something that is connected only with the get.
    *
    *
    * Actually the map operation is a pure function and gives back a par, so only when it will be run
    * there will be the notion of time elapsed.
    *
    *
    * If for example I have the A operation that take 2 seconds, and the B operation that takes 2 seconds,
    * I'll map this two opeation togheter. If this operation are lazy they will be executed outsite the
    * map so nothing to worry about. If these operation are lazy they will be execute as soon we pass an executor
    * service to the parC produced and create a Future.
    *
    *
    * I have to be worried about the time elapsed only for the get operation, because will be the moment
    * were I'll be waitign for some result.
    *
    *
    *
    *
    *
    */
  def map2Timeout[A, B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      TimeAwareMap2Future(af, bf)(f)
    }
  }

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



