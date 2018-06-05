package chapter13

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import org.scalatest.FlatSpec

class ExternalEffectsIOTest extends FlatSpec {

  "The Monad[IO]" should "be able to handle io withou side effect" in {

    def ReadLine: IO[String] = IO {
      /**I shouyld use this, but just using a string scala.io.StdIn.readLine()**/
      "135"
    }
    def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

    def fahrenheitToCelsius(f: Double): Double =
      (f - 32) * 5.0/9.0

    val input = "Hello\nWorld\n"
    val is = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

    def converter: IO[Unit] = for {
      _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
      d <- ReadLine.map(_.toDouble)
      _ <- PrintLine(fahrenheitToCelsius(d).toString)
    } yield ()


    val result = converter //result will held the io without doing any effect until we want!!

    Thread.sleep(1000)
    print("At this point the converted temperature is not printed yet, but we alread executed all the action in the monad")

    //in this way we are able to defer the execution
    result.run

  }

}
