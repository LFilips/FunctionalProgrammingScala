import java.io.File
// This function will evaluate all this step sequentialluy
List(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).map(_ * 3)


false && { println("!!"); true } // does not print anything, it is short circuited

true && { println("!!"); true } // prints the !!




val string = "my name is luca".split(" ")

val interpolated = s"${string(0)} ${string(1)}"


val list = List(1,2,3,4,5,6,7,8,9,10)


val zipped = list.grouped(4).toList

val file = new File("/Users/lfilipponi/projects/playground/FunctionalProgrammingScala/src/main/scala/chapter5/Stream.scala")


file.exists()

