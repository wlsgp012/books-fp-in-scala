package old.ch3.practice


object HasSubSequence extends App {

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Nil, _) => false
    case (_, Nil) => true
    case (_ :: _, h2 :: t2) => sup.contains(h2) && hasSubsequence(sup, t2)
    //    case (h1::t1, h2::t2) => (h1 == h2 || t1.contains(h2)) && hasSubsequence(sup, t2)
  }

  val sup = List(1, 2, 3, 4)
  val sub = List(2, 4)
  val notSub = List(3, 5)

  println(hasSubsequence(sup, sub))
  println(hasSubsequence(sup, notSub))
}
