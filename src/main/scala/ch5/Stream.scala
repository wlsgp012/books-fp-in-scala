package ch5

import ch2.Fibo

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  /**
   * p.89 5.1
   */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /**
   * p.89 5.2
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => Stream.cons(h(), t().take(n - 1))
    case _ => Stream.empty
  }

  /* ??? : 위랑 같은것 아닌가?
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }
   */

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /**
   * p.89 5.3
   */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def exists_(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /**
   * p.91 5.4
   */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  /**
   * p.91 5.5
   */
  def takeWhile_(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)

  /**
   * p.91 5.6
   */
  def headOption_(): Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  /**
   * p.91 5.7
   */
  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, b) => f(a) append b)

  def find(p: A => Boolean): Option[A] = filter(p).headOption


  /*
  Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll.
  The zipAll function should continue the traversal as long as either stream has more elements—it uses Option to indicate
  whether each stream has been exhausted.
   */

  /**
   * p.96 5.13
   */
  def map_[B](f: A => B): Stream[B] = Stream.unfold(this)(s => s match {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  })

  def take_(n: Int): Stream[A] = Stream.unfold((this, n))({
    case (Cons(h, t), m) if m > 0 => Some((h(), (t(), m - 1)))
    case _ => None
  })

  def takeWhile__(p: A => Boolean): Stream[A] = Stream.unfold(this)({
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  })

  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((this, b))({
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, s2))({
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
    case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
    case _ => None
  })

  /**
   * p.96 5.14
   */
  def startsWith[A](s: Stream[A]): Boolean = (zipAll(s)).takeWhile(_._2.isDefined).forAll({ case (s1, s2) => s1 == s2 })

  /**
   * p.97 5.15
   */
  //  def tails: Stream[Stream[A]] = Stream.unfold(this)({
  //    case Cons(h, t) => Some((Cons(h, t), t()))
  //    case _ => None
  //  }) append Stream(Stream.empty)

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(Stream.empty)

  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_.startsWith(s))

  /**
   * p.97 5.16
   */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight(z -> Stream(z))((a, ap) => {
    lazy val accAndCons = ap
    val result = f(a, accAndCons._1)
    (result, Stream.cons(result, accAndCons._2))
  })._2

  // wrong answer
  //  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight(Stream(z))((a, b) => b match {
  //    case Cons(h, _) => Cons(() => f(a, h()), () => b)
  //  })

  def tails_(): Stream[Stream[A]] = scanRight(Stream.empty[A])(Stream.cons(_, _))
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /**
   * p.94 5.8
   */
  //  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  /**
   * p.95 5.9
   */
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  /**
   * p.95 5.10
   */
  def fibs(): Stream[Long] = from(0).map(Fibo.fibTail(_))

  val fibs_from_answer = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  /**
   * p.95 5.11
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Stream.empty[A]
    case Some(v) => Stream.cons(v._1, unfold(v._2)(f))
  }

  /**
   * p.96 5.12
   */
  def fibs_(): Stream[Int] = unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))

  def from_(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def constant_[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

  def ones_(): Stream[Int] = unfold(1)(s => Some(s, s))


  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).take(3).toList)
    println(Stream(1, 2, 3, 4).take_(3).toList)
    println(Stream(1, 2, 3, 4, 5).takeWhile(x => x < 3).toList)
    println(Stream(1, 2, 3, 11, 4, 5).forAll(x => x < 10))

    println(from(0).take(5).toList)
    println(from_(0).take(5).toList)
    println(fibs().take(10).toList)
    println(fibs_().take(10).toList)
    println(fibs_from_answer.take(10).toList)

    println(Stream(1, 2, 3, 11, 4, 5).map(_ * 10).toList)
    println(Stream(1, 2, 3, 11, 4, 5).map_(_ * 10).toList)

    println(Stream(1, 2, 3, 4, 5).startsWith(Stream(1, 2, 3)))
    println(Stream(1).startsWith(Stream(1, 2, 3)))

    println(Stream(1, 2, 3).tails.map(_.toList).toList)
    println(Stream(1, 2, 3).tails_.map(_.toList).toList)

    println(Stream(1, 2, 3).scanRight(0)((a, b) => {
      println(a, b, "+");
      a + b
    }).toList)

  }
}
