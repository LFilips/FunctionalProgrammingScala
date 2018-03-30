package others

object Interleave {

  //todo tail recursive, because the interpolation in calculated in the end
  def interleave(a: String, b: String): String = (a.headOption, b.headOption) match {
    //in case I have head for both the String
    //case (headOptionA,headOptionB) => headOptionA.map((headA) => headOptionB.map((headB) => s"$headA$headB"))
    case (Some(headA), Some(headB)) => s"$headA$headB${interleave(a.tail, b.tail)}"
    case (None, Some(_)) => b
    case (Some(_), None) => a
    case (None, None) => ""
  }


}
