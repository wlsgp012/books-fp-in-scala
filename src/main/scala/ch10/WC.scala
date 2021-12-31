package ch10

sealed trait WC

case class Stub(char: String) extends WC

case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {
  /**
   * p.234 10.10
   */
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def eval(a: String, b: String) = (a + b).trim.length min 1

    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + eval(r1, l2), r2)
      case (Stub(s), Part(l, w, r)) => Part(s + l, w, r)
      case (Part(l, w, r), Stub(s)) => Part(l, w, r + s)
      case (Stub(a), Stub(b)) => Stub(a+b)
    }

    override def zero: WC = Stub("")
  }

}