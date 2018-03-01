
// This function will evaluate all this step sequentialluy
List(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).map(_ * 3)


false && { println("!!"); true } // does not print anything, it is short circuited

true && { println("!!"); true } // prints the !!
