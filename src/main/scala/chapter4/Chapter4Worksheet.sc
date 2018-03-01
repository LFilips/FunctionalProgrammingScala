import chapter4._


List(1) match {
  case h :: t => s"$h $t"
}

List() match {
  case Nil => s"Hi"
  case h :: t => s"$h $t"
}


