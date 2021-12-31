package ch6

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object SimpleRNG {

  /**
   * p.105 6.1
   */
  // inefficient answer
  //  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
  //    case (n, nRNG) if n < 0 => nonNegativeInt(nRNG)
  //    case result => result
  //  }
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  /**
   * p.105 6.2
   */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (1 - i.toDouble / Int.MaxValue, r)
  }

  def double_(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (toBetween01(i), r)
  }

  @tailrec
  def toBetween01(n: Double): Double = {
    if (n < 1) n else toBetween01(n / 10)
  }

  /**
   * p.105 6.3
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    (i, d) -> r2
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (v, r) = intDouble(rng)
    (v.swap, r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    (d1, d2, d3) -> r3
  }

  /**
   * p.105 6.4
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(n: Int, l: List[Int], r: RNG): (List[Int], RNG) = {
      if (n < 1)
        (l, r)
      else {
        val (i, r1) = r.nextInt
        go(n - 1, i :: l, r1)
      }
    }

    go(count, Nil, rng)
  }

  def ints_(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count < 1) (Nil, rng)
    else {
      val (x, r1) = rng.nextInt
      val (xs, r2) = ints_(count - 1)(r1)
      (x :: xs, r2)
    }

  type State[S, +A] = S => (A, S)

  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = (a, _)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  /**
   * p.107 6.5
   */
  def double__ : Rand[Double] = map(nonNegativeInt)(1 - _.toDouble / Int.MaxValue)

  /**
   * p.108 6.6
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (v1, r1) = ra(rng)
    val (v2, r2) = rb(r1)
    (f(v1, v2), r2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  /**
   * p.108 6.7
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List(): List[A]))(map2(_, _)(_ :: _))

  def ints__(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  /**
   * p.110 6.8
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (v, r) = f(rng)
    g(v)(r)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) (mod, _) else nonNegativeLessThan(n)
    //    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  /**
   * p.110 6.9
   */
  def map_[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2_[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

  def main(args: Array[String]): Unit = {
    val rng = new SimpleRNG(17)
    println(double(rng))
    println(double_(rng))
    println(double__(rng))
    println(ints(3)(rng))
    println(ints_(3)(rng))
    println(ints__(3)(rng))
    println(sequence(List(unit(1), unit(2), unit(3)))(rng)._1)
  }
}

/**
 * p.112 6.10
 */
case class State[S, +A](run: S => (A, S)) {
  //    def map[B](f: A => B): State[S,B] = flatMap(a => State(s => (f(a) ,s)))
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (v, s1) = run(s)
    f(v).run(s1)
  })
}

object State {
  def unit[A, S](a: A): State[S, A] = State(s => (a, s))

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] = sas.foldRight(unit(List()): State[S, List[A]])(_.map2(_)(_ :: _))

  def modify[S](f:S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

