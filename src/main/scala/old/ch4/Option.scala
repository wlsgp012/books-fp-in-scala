package old.ch4

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = {
    //    if(map(f) getOrElse false) this
    //    else None
    flatMap(a => if (f(a)) Some(a) else None)
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  //  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match {
  //    case (None, _) => None
  //    case (_, None) => None
  //    case (Some(a), Some(b)) => Some(f(a, b))
  //  }
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap (aa => b map (bb => f(aa, bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

}


