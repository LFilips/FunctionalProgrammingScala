package chapter5

object StrictnessLazyness {

  //I can define a new lazy if2
  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A) : A = {
    if (cond) onTrue else onFalse
  }


}



