
import MyList._
sealed trait MyOption[+A] {
   
   def map[B](f: A => B): MyOption[B] = this match {
      case MyNone => MyNone
      case MySome(a) => MySome(f(a))
   }

   def getOrElse[B >: A](default: => B): B = this match {
      case MyNone => default
      case MySome(a) => a
   }

   def flatMap[B](f: A => MyOption[B]): MyOption[B] =
      map(f) getOrElse MyNone

   def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
      this map(x => MySome(x)) getOrElse ob

   def filter(p: A => Boolean): MyOption[A] =
      flatMap(a => if (p(a)) MySome(a) else MyNone)

}

case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]

object MyOption {

  def mean(xs: Seq[Double]): MyOption[Double] =
    if (xs.isEmpty) MyNone
    else MySome(xs.sum / xs.length)

  def variance(xs: Seq[Double]): MyOption[Double] =
     mean(xs) flatMap (m => mean(xs map (x => math.pow(x - m,2))))

  def lift[A,B](f: A => B): MyOption[A] => MyOption[B] =
      (o: MyOption[A]) => o.map(f)

  def Try[A](a: => A): MyOption[A] =
     try MySome(a)
     catch {case e: Exception => MyNone}

  def map2[A,B,C](a: MyOption[A], b: MyOption[B])(f: (A,B) => C): MyOption[C] =
     a flatMap (aa => (b map (bb => f(aa, bb))))
   
   def sequence[A](a: MyList[MyOption[A]]): MyOption[MyList[A]] =
    a match {
      case Nil => MySome(Nil)
      case Cons(h,t) => h flatMap (hh => sequence(t) map (Cons(hh,_)))
    }
   
    def traverse[A, B](a: MyList[A])(f: A => MyOption[B]): MyOption[MyList[B]] =
    a match {
      case Nil => MySome(Nil)
      case Cons(h,t) => map2(f(h), traverse(t)(f))((x,y) => Cons(x,y))
    }
     
}

