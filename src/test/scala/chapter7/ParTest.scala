package chapter7

import java.util.concurrent._

import org.scalatest.{FlatSpec, Matchers, fixture}
import org.scalamock.scalatest.proxy.MockFactory
import chapter7.Par.{Par, asyncF, fork, lazyUnit, map2, map2Timeout, run, toParOps, unit}


class ParTest extends FlatSpec with Matchers with MockFactory {

  val corePoolSize = 4
  val maximumPoolSize = 4
  //todo val threadFactory = new BasicThreadFactory some libraries can set the executor service pool name
  //val pool: ExecutorService = Executors.newFixedThreadPool(PoolSize) simple instance of executor
  val pool = new ThreadPoolExecutor(corePoolSize, maximumPoolSize,
    0L, TimeUnit.MILLISECONDS,
    new LinkedBlockingQueue[Runnable]());

  //I need this for veryfing the timeout of the map2
  val singleThreadPool = new ThreadPoolExecutor(1, 1,
    0L, TimeUnit.MILLISECONDS,
    new LinkedBlockingQueue[Runnable]());

  /**
    * Simple function which prints the details of the thread
    */
  val threadTracer: (String) => (Long, String) = (method: String) => {
    val currentThread = Thread.currentThread()
    val threadId = currentThread.getId
    val threadName = currentThread.getName
    //println(s"$method $threadId $threadName")
    (threadId, threadName)
  }


  "unit" should "create a synchronous unit of computation, and should be executed on the testing thread" in {

    val par: Par[(Long, String)] = unit(threadTracer("Testing unit function"))

    //this will using the testing thread
    val future = Par.run(pool)(par)

    //future.get()._1 should be(1) //I can't assert on ThreadId, the test framework will parallelize this
    future.get()._2 should include regex "ScalaTest*" //scalatest thread

  }

  it should "synchronously (immediately) start the computation " in {

    val stubbedFunction = stubFunction[Int, String]

    unit(stubbedFunction(3))

    stubbedFunction.verify(3)

  }


  it should "execute the computation synchrously blocking the thread, timeout shouldn't occur" in {

    val function = () => {
      Thread.sleep(2000);
      "done"
    }

    val par = unit(function())

    val future = Par.run(pool)(par)

    future.get(10, TimeUnit.NANOSECONDS) should be("done")


  }

  "fork" should "create a new par which does the computation in a different thread" in {

    val par: Par[(Long, String)] = lazyUnit(threadTracer("Testing unit function"))

    //this will using the testing thread
    val future = Par.run(pool)(par)

    future.get()._2 should startWith regex "pool-*" //TODO use a custom pool name?

  }

  "lazyUnit" should "not start the computation till is run" in {

    //lazy unit is taking a lazy param, so the function is not evaluated until we call the run

    val stubbedFunction = stubFunction[Int, String]

    lazyUnit(stubbedFunction(3))

    stubbedFunction.verify(3).never()

  }

  it should "start delay the computation until the run command" in {

    //lazy unit is taking a lazy param, so the function is not evaluated until we call the run

    val stubbedFunction = stubFunction[Int, String]

    stubbedFunction.when(3).returns("called")

    val par = lazyUnit(stubbedFunction(3))

    val future: Future[String] = Par.run(pool)(par)

    future.get should be("called") //waiting for the task to complete, otherwise the test can fail

    stubbedFunction.verify(3)

  }

  it should "give back a future which used timeouts" in {

    val function = () => {
      Thread.sleep(2000);
      "done"
    }

    val par = lazyUnit(function())

    val future = Par.run(pool)(par)

    an[TimeoutException] should be thrownBy future.get(10, TimeUnit.NANOSECONDS)

  }


  "unit" should "start the computation synchronously immediatly" in {

    val stubbedFunction = stubFunction[Int, String]

    unit(stubbedFunction(3))

    stubbedFunction.verify(3)

  }

  "map2" should "map two par computation together and apply a function to the result" in {

    val parA = unit(3)
    val parB = lazyUnit(4) //it is lazy because the argument is not evaluated and is computed in a different thread

    val parC = map2(parA, parB)((a, b) => a + b)

    Par.run(pool)(parC).get should be(7)

  }

  it should "not respect the timeout considering both computation with lazyUnit/lazyUnit" in {

    val sleepABit = (milliseconds: Long) => {
      Thread.sleep(milliseconds);
      "done"
    }

    val parA = lazyUnit(sleepABit(500))
    val parB = lazyUnit(sleepABit(1000))

    val parC = map2(parA, parB)((a, b) => a + b)

    val future = Par.run(singleThreadPool)(parC)

    future.get(1200, TimeUnit.MILLISECONDS) should be("donedone")

  }

  "map2Timeout" should "respect the timeout considering both computation with lazyUnit/lazyUnit" in {

    val sleepABit = (milliseconds: Long) => {
      Thread.sleep(milliseconds);
      "done"
    }

    val parA = lazyUnit(sleepABit(500))
    val parB = lazyUnit(sleepABit(1000))

    val parC = map2Timeout(parA, parB)((a, b) => a + b)

    /**
      * I need to use the single thread pool otherwise the computation will be executed
      * in parallel and the time consumed during A will used to do B and I will never
      * be in the situation of a timeout
      */

    val future = Par.run(singleThreadPool)(parC)

    an[TimeoutException] should be thrownBy future.get(1200, TimeUnit.MILLISECONDS)

  }


  it should "not consider the time elapsed during the Unit since it is synchronous" in {

    val sleepABit = (milliseconds: Long) => {
      Thread.sleep(milliseconds);
      "done"
    }

    val parA = unit(sleepABit(1250)) // the sleep will occur here, so there will be no time consumed by it
    val parB = lazyUnit(sleepABit(1000))

    val parC = map2Timeout(parA, parB)((a, b) => a + b)

    val future = Par.run(singleThreadPool)(parC)

    future.get(1500, TimeUnit.MILLISECONDS) should be("donedone")

  }

  "asyncF" should "convert any function to an asynchronous one" in {

    val functionToExecuteAsync = threadTracer

    val asyncFunction = Par.asyncF(threadTracer)

    //this will using the testing thread
    val result = Par.run(singleThreadPool)(asyncFunction("hi"))

    result.get()._2 should startWith regex "pool-*" //part of the thread pool

  }

  "map2" should "be called with the infix operator though the implicit conversion" in {

    val parA = unit(3)
    val parB = lazyUnit(4) //it is lazy because the argument is not evaluated and is computed in a different thread


    val parC = parA.map2(parB)((a, b) => a + b)

    Par.run(pool)(parC).get should be(7)

  }


  "sequence" should "convert a List[Par[A]] in a Par[List[A]" in {

    val list = List.tabulate(10)(lazyUnit(_))

    val sequenced = Par.sequence(list)

    Par.run(pool)(sequenced).get() should be(List.tabulate(10)((x) => x))

  }

  "parMap" should "apply the map operation in parallel" in {

    val list = List.tabulate(10)((x) => x)

    val parMapped = Par.parMap(list)((x) => (x * 2).toString)

    Par.run(pool)(parMapped).get() should be(List.tabulate(10)((x) => (x * 2).toString))

  }

  "parMap" should "apply the map operation in parallel 2" in {

    val list = List.tabulate(10)((x) => x)

    val parMapped = Par.parMap(list)((x) => (x * 2).toString)

    Par.runDebug(pool)(parMapped).get() should be(List.tabulate(10)((x) => (x * 2).toString))

  }

  "parFilter" should "apply the filter operation in parallel" in {

    val list = List.tabulate(20)((x) => x)

    val parFiltered = Par.parFilter(list)((x) => (x % 2) == 0) //Filtering even number

    Par.run(pool)(parFiltered).get() should be(List.tabulate(10)((x) => x * 2))

  }

  /**
    * This test shows that we have a deadlock when we use a singlethread pool
    * becuase of the implementation of fork
    */
  ignore should "cause a deadlock with a singlethread pool" in {

    val a = lazyUnit(42 + 1)
    val S = Executors.newFixedThreadPool(1)
    Par.equal(S)(a, fork(a))

    /** What is happening here?
      * This is the equals expanded:
      * p(e).get == p2(e).get
      *
      * The executor is passed to a(e).get and it blocks and gets the result.
      * The second part is to do p2(e).get where p2 is a fork.
      * So we have a fork(a)(e).get, but the inner implementation of the fork is using a thread for submitting
      * the job and another for waiting and with a thread pool of 1 we this will cause a deadlock
      *
      */

  }

  /**
    * becuase of the implementation of fork
    */
  ignore should "cause a deadlock with a sinngle thead pool without using the equal" in {

    val a = lazyUnit(42 + 1)
    val S = Executors.newFixedThreadPool(1)
    fork(a)(S).get

  }

  ignore should "cause a deadlock with any thead pools if met certaing condition" in {

    /**
      * a is executing the get on B in is own get, since all the thread are blocked the new job
      * in fork cant be submitted
      */

    val b = lazyUnit(42 + 1)
    val S = Executors.newFixedThreadPool(2)
    val a = lazyUnit(Par.run(S)(b).get)
    fork(a)(S).get

  }


  /**
    * This unit tests shows how you can create a cyclic dependency between job and consume the thread pool causing deadlock
    */
  ignore should "cause a deadlock with a any fixed size pool" in {

    val PoolSize = 5

    val fixedSizePool = Executors.newFixedThreadPool(PoolSize)

    val firstJob = lazyUnit(42 + 1)


    def createCyclicDep(p: Par[Int], size: Int): Par[Int] = size match {
      case 0 => p
      case n => createCyclicDep(lazyUnit(Par.run(fixedSizePool)(firstJob).get), size)
    }

    val cyclicJob = createCyclicDep(firstJob, PoolSize)

    fork(cyclicJob)(fixedSizePool).get

  }


  "choice" should "choose the function to executed according the result of the first" in {

    val truePar = lazyUnit(true)
    val falsePar = lazyUnit(false)
    val t = lazyUnit("true")
    val f = lazyUnit("false")
    val trueChosenPar = Par.choice(truePar)(t, f)
    val falseChosenPar = Par.choice(falsePar)(t, f)

    trueChosenPar(pool).get() should be("true")
    falseChosenPar(pool).get() should be("false")

  }

  "choice2" should "choose the function to executed according the result of the first" in {

    val truePar = lazyUnit(true)
    val falsePar = lazyUnit(false)
    val t = lazyUnit("true")
    val f = lazyUnit("false")
    val trueChosenPar = Par.choice2(truePar)(t, f)
    val falseChosenPar = Par.choice2(falsePar)(t, f)

    trueChosenPar(pool).get() should be("true")
    falseChosenPar(pool).get() should be("false")

  }



  "choice3" should "choose the function to executed according the result of the first" in {

    val truePar = lazyUnit(true)
    val falsePar = lazyUnit(false)
    val t = lazyUnit("true")
    val f = lazyUnit("false")
    val trueChosenPar = Par.choice3(truePar)(t, f)
    val falseChosenPar = Par.choice3(falsePar)(t, f)

    trueChosenPar(pool).get() should be("true")
    falseChosenPar(pool).get() should be("false")

  }

  "choiceN" should "choose the function to executed according the result of the first" in {

    val conditionPar = lazyUnit(1)
    val list = List(lazyUnit("one"),lazyUnit("two"))
    val chosenPar = Par.choiceN(conditionPar)(list)

    chosenPar(pool).get() should be("two")

  }

  "flatMap" should "apply the function f to A and the flatten the result in a Par[B]" in {

    //using implicit conversion for getting the flatmap
    val par = lazyUnit(1)
    par.flatMap((a) => lazyUnit(a*2))(pool).get should be(2)

  }

  "flatMapUsingJoin" should "apply the function f to A and the flatten the result in a Par[B]" in {

    //using implicit conversion for getting the flatmap
    val par = lazyUnit(1)
    par.flatMapUsingJoin((a) => lazyUnit(a*2))(pool).get should be(2)

  }

  "join" should "extract the Par inside the Par" in {

    //using implicit conversion for getting the flatmap
    val par = lazyUnit(lazyUnit(1))
    Par.join(par)(pool).get() should be(1)

  }

}
