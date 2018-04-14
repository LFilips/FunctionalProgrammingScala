/**
  * Worksheet for understanding the for comprehension
  * *
  * Every datatype that supports the operations withFilter, map, and flatMap
  * (with the proper types) can be used in sequence comprehensions.
  **/


/**
  *
  * Using for comprehension for normal operation
  *
  */


// let say we have three option
val option = Some(3)
val option2 = Some(4)
val option3 = Some(5)

for {
  o1 <- option
  o2 <- option2
  o3 <- option3
  //d <- Some(o1 + o2 + o3)

} yield o3 + o2 + o1

/** The for comprehension is just synctict sugar for this*/

option.flatMap { o1 =>
  option2.flatMap { o2 =>
    option3.map { o3 =>
      o1 + o2 + o3
    }
  }
}


