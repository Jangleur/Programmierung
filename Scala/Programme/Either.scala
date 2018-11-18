sealed trait MyEither[+E, +A] {
  def map[B](f: A => B): MyEither[E, B] = this match {
      case MyLeft(e) => MyLeft(e)
      case MyRight(a) => MyRight(f(a))
   }
  
  def flatMap[EE >: E, B](f: A => MyEither[EE,B]): MyEither[EE, B] = this match {
     case MyLeft(e) => MyLeft(e)
     case MyRight(a) => f(a)
  }
 
  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
     case MyLeft(_) => b
     case MyRight(a) => MyRight(a)
  }

  def map2[EE >: E, B, C](b: MyEither[EE,B])(f: (A,B) => C): MyEither[EE, C] =
     this flatMap (a => (b map (bb => f(a,bb))))
    
}
case class MyLeft[+E](value: E) extends MyEither[E,Nothing]
case class MyRight[+A](value: A) extends MyEither[Nothing, A]

object MyEither {

   def mean(xs: Seq[Int]): MyEither[String, Double] = 
      if (xs.isEmpty) 
         MyLeft("mean of empty list")
      else
         MyRight(xs.sum/xs.length)

   def Try[A](a: => A): MyEither[Exception, A] =
     try MyRight(a)
     catch {case e: Exception => MyLeft(e)}

   
}
