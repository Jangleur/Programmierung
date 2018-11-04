sealed trait MyOption[+A] {
   
   def map[B](f: A => B): MyOption[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
   }

   def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
   }

   def flatMap[B](f: A => MyOption[B]): MyOption[B] =
      map(f) getOrElse None

   def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
      this map(x => Some(x)) getOrElse ob

   def filter(p: A => Boolean): MyOption[A] =
      flatMap(a => if (p(a)) Some(a) else None)

}

case class Some[+A](get: A) extends MyOption[A]
case object None extends MyOption[Nothing]

object MyOption {

  def mean(xs: Seq[Double]): MyOption[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): MyOption[Double] =
     mean(xs) flatMap (m => mean(xs map (x => math.pow(x - m,2))))

  def lift[A,B](f: A => B): MyOption[A] => MyOption[B] =
      (o: MyOption[A]) => o.map(f)
}

