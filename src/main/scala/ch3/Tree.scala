package ch3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /**
   * p.57 3.25
   */
  //  def size[A](t: Tree[A]):Int = {
  //    def go(x: Tree[A], n: Int):Int = x match {
  //      case Leaf(_) => n+1
  //      case Branch(l, r) => go(l,n) + go(r, n) + 1
  //    }
  //    go(t, 0)
  //  }
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  /**
   * p.57 3.26
   */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  /**
   * p.57 3.27
   */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  /**
   * p.57 3.28
   */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /**
   * p.58 3.29
   */
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size_[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

  def maximum_(t: Tree[Int]): Int = fold(t)(v => v)(_ max _)

  def depth_[A](t: Tree[A]): Int = fold(t)(_ => 0)((r, l) => (r max l)+1)

  def map_[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))

  def main(args: Array[String]): Unit = {
    println(size(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))))
    println(size_(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))))
    println(maximum(Branch(Branch(Leaf(1), Branch(Leaf(9), Leaf(3))), Leaf(4))))
    println(maximum_(Branch(Branch(Leaf(1), Branch(Leaf(9), Leaf(3))), Leaf(4))))
    println(depth(Branch(Branch(Leaf(1), Branch(Leaf(9), Leaf(3))), Leaf(4))))
    println(depth_(Branch(Branch(Leaf(1), Branch(Leaf(9), Leaf(3))), Leaf(4))))
    println(map(Branch(Branch(Leaf(1), Branch(Leaf(9), Leaf(3))), Leaf(4)))(_ * 10))
    println(map_(Branch(Branch(Leaf(1), Branch(Leaf(9), Leaf(3))), Leaf(4)))(_ * 10))
  }
}

