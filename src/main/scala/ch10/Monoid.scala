package ch10

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero: List[A] = Nil
  }

  /**
   * p.227 10.1
   */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  /**
   * p.227 10.2
   */
  def optionMonoid[A] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def zero: Option[A] = None
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero = m.zero
  }

  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]

  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  /**
   * p.228 10.3
   */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 compose a2

    override def zero: A => A = identity
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  /**
   * p.230 10.5
   */
  // my answer
  //  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as.map(f), m)
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  /**
   * p.230 10.6
   */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B])(a => b => f(a, b))(z)

  /**
   * p.232 10.7
   */
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.size match {
    case 0 => m.zero
    case 1 => f(v.head)
    case _ =>
      val (l, r) = v.splitAt(v.size / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
  }

  /**
   * p.237 10.15
   */
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1 , a2._1), B.op(a1._2, a2._2))

    override def zero: (A, B) = (A.zero, B.zero)
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] = (a1.keySet ++ a2.keySet).foldLeft(zero){
      (acc, k) => acc.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
    }

    override def zero: Map[K, V] = Map[K, V]()
  }

  /**
   * p.238 10.17
   */
  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: A => B, a2: A => B): A => B = x => B.op(a1(x), a2(x))

    override def zero: A => B = _ => B.zero
  }

  def main(args: Array[String]): Unit = {
    val words = List("Hic", "Est", "Index")
    val s = words.foldRight(stringMonoid.zero)(stringMonoid.op)
    println(s)
    val t = words.foldLeft(stringMonoid.zero)(stringMonoid.op)
    println(t)
    val f = (x: Int, y: Int) => x + 1 + y
    f.curried
    println("============")
    val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))
    val m1: Map[String, Map[String, Int]] = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
    val m2: Map[String, Map[String, Int]] = Map("o1" -> Map("i2" -> 3))
    val m3 = M.op(m1, m2)
    println(m3)
  }
}