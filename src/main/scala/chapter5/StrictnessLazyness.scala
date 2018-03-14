package chapter5

object StrictnessLazyness {

  //I can define a new lazy if2
  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A = {
    if (cond) onTrue else onFalse
  }

  def maybeTwice(b: Boolean, i: => Int) = if (b) i + i else 0

  def maybeTwice2(b: Boolean, i: => Int) = {
    lazy val j = i //Adding lazy keyword to declaration will cause Scala to delay evaluation of the right-hand side of
    // that lazy val declaration until it’s first referenced.
    // It will also cache the result so that subsequent references to it don’t trigger repeated evaluation.
    if (b) j + j else 0
  }
}

//Runnable object, extends app which has a def main(args: Array[String]) = {
object Run extends App {

  import StrictnessLazyness._

  println("MaybeTwice ...")
  maybeTwice(true, {
    println("hi"); 1 + 41
  }) //this is printed twice!!

  println("MaybeTwice2 ...")
  maybeTwice2(true, {
    println("hi"); 1 + 41 //this is printed only once, because lazy caches the result
  })

}

