package ch4

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

sealed trait Option[+A] {
  /**
   * p.67 4.1
   */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  //  def filter(f: A=> Boolean): Option[A] = if(map(f) getOrElse false) this else None
  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
   * p.68 4.2
   */
  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  /**
   * p.72 4.3
   */
  //  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.map(x => lift((y: B) => f(x, y))).flatMap(g => g(b))
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap (aa => b map (bb => f(aa, bb)))

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = numberOfSpeedingTickets / age

  def Try[A](a: => A): Option[A] = try Some(a) catch {
    case e: Exception => None
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  /**
   * p.73 4.4
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldRight(Some(List()): Option[List[A]])((vo, lo) => map2(vo, lo)(_ :: _))

  //  def sequence_[A](a: List[Option[A]]): Option[List[A]] = a match {
  //    case Nil => Some(Nil: List[A])
  //    case None :: _ => None
  //    case x :: xs => map2(x, sequence_(xs))(_ :: _)
  //  }
  def sequence_[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil: List[A])
    case x :: xs => x flatMap (h => sequence_(xs) map (h :: _))
  }

  /**
   * p.74 4.5
   */
  //  def traverse_[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(a map f)
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x :: xs => f(x) flatMap (h => traverse(xs)(f) map (h :: _))
    //    case x::xs => map2(f(x), traverse(xs)(f))(_ :: _)
  }

  def traverse_1[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

}

