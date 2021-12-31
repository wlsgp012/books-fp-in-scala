package ch8

import ch6.{RNG, SimpleRNG, State}
import ch7.Par
import ch7.Par.Par
import ch8.Gen.{apply => _, unapply => _, _}
import ch8.Prop._

import java.util.concurrent.{ExecutorService, Executors}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  /**
   * p.163 8.3
   */
  // My answer was wrong.
  //  def &&(p: Prop): Prop = if(this.check) p else this
  //  def &&(p: Prop): Prop = new Prop {
  //    def check = Prop.this.check && p.check
  //  }

  /**
   * p.170 8.9
   */
  def &&(p: Prop): Prop = Prop {
    (m, n, r) =>
      run(m, n, r) match {
        case Passed | Proved => p.run(m, n, r)
        case x => x
      }
  }

  def ||(p: Prop): Prop = Prop {
    (m, n, r) =>
      run(m, n, r) match {
        case Falsified(msg, _) => p.tag(msg).run(m, n, r)
        case x => x
      }
  }

  def tag(msg: String) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }


  def apply(f: (TestCases, RNG) => Result): Prop = Prop { (_, n, rng) => f(n, rng) }

  //  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
//          println(s"value is $a")
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop { (max, _, rng) => p.run(max, casesPerSize, rng) }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def buildMsg[A](s: A, e: Exception): String = s"test case: $s\n" + s"generated an exception: ${e.getMessage}\n" + s"stack strace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ Ok, proved property.")
    }

  //  def check(p: => Boolean): Prop = {
  //    lazy val result = p
  //    forAll(unit(()))(_ => result)
  //  }
  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p, p2)(_ == _)

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
    unit(Executors.newCachedThreadPool) -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
  //    forAll(S.map2(g)((_, _))) { case (s, a) => f(a)(s).get }
    forAll(S ** g) { case s ** a => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop = forAllPar(Gen.unit(()))(_ => p)

  def main(args: Array[String]): Unit = {
    val smallInt = choose(-10, 10)
    val maxProp = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    run(maxProp)

    println("-------------------------------")
    /**
     * p.175 8.14
     */
    val sortedProps = forAll(listOf(smallInt)) { ns =>
      val nss = ns.sorted
      (nss.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists {
        case (a, b) => a > b
      }) && ns.forall(nss.contains(_)) && nss.forall(ns.contains(_))
    }
    run(sortedProps)

    println("-------------------------------")

    val ES: ExecutorService = Executors.newCachedThreadPool
    val p1 = forAll(unit(Par.unit(1)))(i => Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)
    val p2 = check {
      val p = Par.map(Par.unit(1))(_ + 1)
      val p2 = Par.unit(2)
      p(ES).get == p2(ES).get
    }
    val p2_ = checkPar {
      equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )
    }
    val p3 = check {
      equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )(ES).get
    }
    val pint: Gen[Par[Int]] = Gen.choose(0, 10) map (Par.unit(_))
    val p4 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))
    run(p4)


    println("-------------------------------")
    /**
     * p.181 8.16
     */
    val pint2: Gen[Par[Int]] =
      choose(-100, 100)
        .listOfN(choose(0, 20))
        .map(l =>
          l.foldLeft(Par.unit(0))((p,i) =>
            Par.fork { Par.map2(p, Par.unit(i))(_ + _) })
        )
    /**
     * p.181 8.17
     */
    val forkProp = forAllPar(pint2)(i => equal(Par.fork(i), i)) tag "fork"

    println("-------------------------------")

    val isEven = (i: Int) => i % 2 == 0
    val takeWhileProp = forAll(listOf(smallInt))(ns => ns.takeWhile(isEven).forall(isEven))
    run(takeWhileProp)


  }
}


case class Gen[A](sample: State[RNG, A]) {
  /**
   * p.167 8.6
   */
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(this.sample.flatMap(s => f(s).sample))

  def map[B](f: A => B): Gen[B] = Gen(this.sample.map(f))

  //  def map2[B,C](g: Gen[B])(f: (A,B) =>C): Gen[C] = flatMap(a => Gen(g.sample.map(b => f(a,b))))
  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = Gen(sample.map2(g.sample)(f))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  //  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(s => Gen(State.sequence(List.fill(s)(this.sample))))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(s => Gen.listOfN(s, this))

  /**
   * p.167 8.7
   */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  /**
   * p.172 8.10
   */
  def unsized: SGen[A] = SGen(n => this)

  /**
   * p.172 8.12
   */
  //  def listOf[A](g: Gen[A]): SGen[List[A]]= SGen(n => g.listOfN(n))
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(unit(n)))

  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g) ((_, _))

}

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)
}


object Gen {
  /**
   * p.163 8.4
   */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(SimpleRNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  /**
   * p.163 8.5
   */
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(SimpleRNG.nonNegativeInt).map(i => i % 2 == 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))

  /**
   * p.167 8.13
   */
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n max 1))

  /**
   * p.167 8.8
   */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1, w1) = g1
    val (gen2, w2) = g2
    //    val norm = w1/(w1+w2)
    val norm = w1.abs / (w1.abs + w2.abs)
    Gen(State(SimpleRNG.double).flatMap(r => if (norm > r) gen1.sample else gen2.sample))
  }


}

object ** {
  def unapply[A, B](p: (A, B)) = Some(p)
}
