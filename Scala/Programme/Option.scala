sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None
  
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  } 
  
  def orElse[B >: A](obj: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse obj	

  def filter(p: A => Boolean): Option[A] = this match {
    case Some(a) if p(a) => Some(a)
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

