trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A] (head: A, tail: MyList[A]) extends MyList[A]

object MyList { 
  def sum(ints: MyList[Int]): Int = ints match { 
    case Nil => 0 
    case Cons(x,xs) => x + sum(xs) 
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def drop[A](l: MyList[A], n: Int): MyList[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }
  }

  def dropWhile[A](l: MyList[A], p: A => Boolean): MyList[A] = l match {
     case Cons(h, t) if p(h) => dropWhile(t, p)
     case _ => l
  }

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def apply[A](as: A*): MyList[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}


