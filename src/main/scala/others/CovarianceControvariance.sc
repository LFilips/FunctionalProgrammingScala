
//some test on variant, covariant and controvariant


class Cell[T](init: T) {
  private[this] var current = init

  def get = current

  def set(x: T) = {
    current = x
  }
}


val c1 = new Cell[String]("abc")

// val c2 : Cell[Any] = c1 I get an error on this line, because cell is nonvariant, so I can't have subtyping on Cell
// c2.set(1)
// val s: String = c1.get


/**
  * I can define a cell type that, is covariant with the +T, so I can use subtyping with the Cell
  *
  */
class CellCovariant[+T](init: T) {
  private[this] var current = init

  def get = current

  /** def set(x: T) = { current = x}
    *
    * This line need to be commented, because I have this error:
    *
     cmd2.sc:4: covariant type T occurs in contravariant position in type T of value x
     def set(x: T) = { current = x}
     ^

     Compilation Failed

    * What does this mean?
    *
    * Making test covariant in T means that Test[A] is a subtype of Test[Any] for any A. So lets create a Test:
    *
    * */

  class Test[+T] {
    //var list: List[T] = _
  }
  val test_string = new Test[String]
  val test_any : Test[Any] = test_string
  //test_any.list = List[Any]()

  /**
    *
    *
    * The point is that a controvariant position is when you a using the type
    * in a way that you can have problem when using sub type,the upper example would have been working if was a var instead of a val
    *
    */

  class Test2[+T] {
    val list: List[T] = _ //this is okay, since we can't assign again a value to it

    //def method(x: T) = {} //Scala has strict rules for this, even if the method is doing
  }

  /**
    *
    *
    * The scala compiler rules for the covariance or controvariance are complicatd, but essentially the code has some position that are
    * marked as positive or negative, and the compiler checks that positive or negative type + or - T are not in that position and they
    * are conform to that definition. All of this is necessary for avoiding potential error at runtime with type.
    *
    */


  /**
    *
    * A similar behaviour can be obtained with java in this way:
    * *
    *
    * String[] a1 = { "abc" };
    * Object[] a2 = a1;
    * a2[0] = new Integer(17);
    * String s = a1[0];
    * *
    * Where i will have an ArrayStoreException
    * This happen because java is storing the value of the array at runtime, and in this case is instantiated to a String[]
    * but then an Object[] point to that (it is legal to do) and then we store an Int in it.
    * *
    * Array in java are covariant, this mean that if A subtype of B, then Array[A] is subtype of B, this was done beacuse
    * of the sorting method of the array.
    * *
    * void sort(Object[] a, Comparator cmp) { ... }
    * *
    * in this way was possible to assing an Object[] a = b (where b is a String[])
    * *
    * The covariance allows you to do more general method
    *
    *
    */


}