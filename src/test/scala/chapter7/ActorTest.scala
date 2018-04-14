package chapter7


import java.util.concurrent.Executors
import com.typesafe.scalalogging.Logger
import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.LoggerFactory

class ActorTest extends FlatSpec with Matchers {

  val logger = Logger(LoggerFactory.getLogger(classOf[ActorTest]))

  val Pool = Executors.newFixedThreadPool(4)


  "actor" should "act" in {
    val echoer = Actor[String](Pool) {
      (msg) => logger.debug(msg)
    }
    echoer ! "hi"
    echoer ! "I will repeat"
    echoer ! "everything on the logger"
  }
}
