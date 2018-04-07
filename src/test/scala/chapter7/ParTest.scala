package chapter7

import java.util.concurrent._

import org.scalatest.{FlatSpec, Matchers, fixture}
import org.scalamock.scalatest.proxy.MockFactory
import chapter7.Par.{Par, map2, run, unit, lazyUnit,map2Timeout}

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
    println(s"$method $threadId $threadName")
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

    val function = () => {Thread.sleep(2000);"done"}

    val par = unit(function())

    val future = Par.run(pool)(par)

    future.get(10,TimeUnit.NANOSECONDS) should be("done")


  }

  "fork" should "create a new par which does the computation in a different thread" in {

    val par: Par[(Long, String)] = lazyUnit(threadTracer("Testing unit function"))

    //this will using the testing thread
    val future = Par.run(pool)(par)

    future.get()._2 should startWith regex "pool-*" //custon pool name

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

    val future : Future[String] = Par.run(pool)(par)

    future.get should be ("called")//waiting for the task to complete, otherwise the test can fail

    stubbedFunction.verify(3)

  }

  it should "give back a future which used timeouts" in {

    val function = () => {Thread.sleep(2000);"done"}

    val par = lazyUnit(function())

    val future = Par.run(pool)(par)

    an [TimeoutException] should be thrownBy future.get(10,TimeUnit.NANOSECONDS)

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

  "map2Timeout" should "respect the timeout considering both computation with lazyUnit/lazyUnit" in {

    val sleepABit = (milliseconds: Long) => {Thread.sleep(milliseconds);"done"}

    val parA = lazyUnit(sleepABit(500))
    val parB = lazyUnit(sleepABit(1000))

    val parC = map2Timeout(parA, parB)((a, b) => a + b)

    /**
      *I need to use the single thread pool otherwise the computation will be executed
      * in parallel and the time consumed during A will used to do B and I will never
      * be in the situation of a timeout
      */

    val future = Par.run(singleThreadPool)(parC)

    an [TimeoutException] should be thrownBy future.get(1200,TimeUnit.MILLISECONDS)

  }


  "map2Timeout" should "not consider the time elapsed during the Unit since it is synchronous" in {

    val sleepABit = (milliseconds: Long) => {Thread.sleep(milliseconds);"done"}

    val parA = unit(sleepABit(1250)) // the sleep will occur here, so there will be no time consumed by it
    val parB = lazyUnit(sleepABit(1000))

    val parC = map2Timeout(parA, parB)((a, b) => a + b)

    val future = Par.run(pool)(parC)

    future.get(1500,TimeUnit.MILLISECONDS) should be("donedone")

  }


}
