package ch3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    @tailrec
    def go(l: Seq[A], acc: List[A]): List[A] = {
      if (l.isEmpty) acc else go(l.tail, Cons(l.head, acc))
    }

    go(as.reverse, Nil: List[A])
  }

  //  def apply[A](as: A*): List[A] =
  //    if (as.isEmpty) Nil
  //    else Cons(as.head, apply(as.tail: _*))

  /**
   * p.45 3.2
   */
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  /**
   * p.45 3.3
   */
  def setHead[A](list: List[A], alter: A) = list match {
    case Nil => Nil
    case Cons(_, t) => Cons(alter, t)
  }

  /**
   * p.46 3.4
   */
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else drop(tail(l), n - 1)
  }

  /**
   * p.46 3.5
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def dropWhile2[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile2(t, f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  /**
   * p.47 3.6
   */
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  //  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  /**
   * p.51 3.9
   */
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, y) => y + 1)

  /**
   * p.51 3.10
   */
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  /**
   * p.52 3.11
   */
  def suml(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def productl(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def lengthl[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  /**
   * p.52 3.12
   */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))

  /**
   * p.52 3.13
   */
  def foldRight_[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, (b: B) => b)((acc, x) => b => acc(f(x, b)))(z)

  def foldLeft_[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as, (b: B) => b)((x, acc) => b => acc(f(b, x)))(z)

  /**
   * p.52 3.14
   */
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((x, acc) => Cons(x, acc))

  /**
   * p.52 3.15
   */
  def flatten[A](a: List[List[A]]): List[A] = foldRight(a, Nil: List[A])((x, acc) => append(x, acc))

  /**
   * p.53 3.16
   */
  def plusOne(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((x, acc) => Cons(x + 1, acc))

  /**
   * p.53 3.17
   */
  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((x, acc) => Cons(x.toString, acc))

  /**
   * p.53 3.18
   */
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((x, acc) => Cons(f(x), acc))

  /**
   * p.53 3.19
   */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

  /**
   * p.53 3.20
   */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  /**
   * p.53 3.21
   */
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if(f(x)) List(x) else List())

  /**
   * p.54 3.22
   */
  def listSum(a: List[Int], b: List[Int]):List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1),Cons(h2, t2)) => Cons(h1+h2, listSum(t1, t2))
  }

  /**
   * p.54 3.23
   */
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C):List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1),Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
  }


  /**
   * p.55 3.24
   */
   /**
    * 틀린답
  def contains[A](e: A)(l: List[A]): Boolean = foldLeft(l, false)((b, x) => if(b) b else x == e)
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = foldLeft(sub, true)((b, x) => b && contains(x)(sup))
  */
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_,t) => hasSubsequence(t, sub)
  }

  def main(args: Array[String]): Unit = {
    //    println(dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x <= 3))
    //    println(init(List(1, 2, 3, 4, 5)))
    //    println(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
    //    println(product2(List(1, 2, 3, 4)))
    //    println(productl(List(1, 2, 3, 4)))
    //    println(length(List(1, 2, 3, 4)))
    //    println(reverse(List(1, 2, 3, 4)))
    //    println(appendViaFoldRight(List(1,2), List(3,4)))
    //    val smallList = List((1 to 100): _*)
    //    println(foldLeft(smallList, 0)(_ - _))
    //    println(foldLeft_(smallList, 0)(_ - _))
    //    println(foldRight(smallList, 0)(_ - _))
    //    println(foldRight_(smallList, 0)(_ - _))
    //
    //    println(flatten(List(List(0), List(1, 2), List(2, 3, 4))))

    //    val bigList = List((1 to 1000000): _*)
    //    println(foldLeft(bigList, 0)(_ + _))
    //    println(foldRight(bigList, 0)(_ + _))

//    println(filter(List(1, 2, 3, 4))(x => x % 2 == 0))
//    println(contains(0)(List(1,2,3,4)))
//    println(contains(2)(List(1,2,3,4)))

    println(hasSubsequence(List(1,2,3,4), List(3,4)))
    println(hasSubsequence(List(1,2,3,4), List(4, 2)))
  }

}
