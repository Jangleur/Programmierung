trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A] (head: A, tail: MyList[A]) extends MyList[A]

object MyList { 

  def tail[A](l: MyList[A]): MyList[A] = l match {
    case Nil => Nil
    case Cons(h,t) => t
  }

  def sum(ints: MyList[Int]): Int = ints match { 
    case Nil => 0 
    case Cons(x,xs) => x + sum(xs) 
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def foldRight[A,B](l: MyList[A], ne: B)(f:(A,B) => B): B = l match {
    case Nil => ne
    case Cons(h,t) => f(h, foldRight(t,ne)(f))
  }

  @annotation.tailrec
  def foldLeft[A,B](l: MyList[A], ne: B)(f:(B,A) => B): B = l match {
    case Nil => ne
    case Cons(h,t) => foldLeft(t, f(ne, h))(f)
  }

  def map[A,B](l: MyList[A])(f: A => B): MyList[B] =
    foldLeft(reverse(l),MyList[B]())((x,y) => Cons(f(y),x))

  def filter[A](l: MyList[A])(f: A => Boolean): MyList[A] =
    flatMap(l)(x => if (f(x)) MyList[A](x) else MyList[A]())

  def flatMap[A,B](l: MyList[A])(f: A => MyList[B]): MyList[B] =
    foldLeft(reverse(l), MyList[B]())((x,y) => concate(f(y),x))

  def zipWith[A, B, C](a1: MyList[A], a2: MyList[B])(f: (A,B) => C): MyList[C] = (a1,a2) match {
    case (_,Nil) => Nil
    case (Nil,_) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = (sup,sub) match {
    case (_,Nil) => true
    case (Nil,_) => false
    case (Cons(h1,t1), Cons(h2,t2)) => if (h1==h2) hasSubsequence(t1,t2) else hasSubsequence(t1,Cons(h2,t2))
  }
  	
  def concate[A](l: MyList[A], a: MyList[A]): MyList[A] =
    foldLeft(l, a)((x,y) => Cons(y,x))
  	
  def reverse[A](l: MyList[A]): MyList[A] = 
    foldLeft(l,MyList[A]())((x,y) => Cons(y,x))
 
  def foldRight2[A,B](l: MyList[A], ne: B)(f: (A,B) => B): B = 
    foldLeft(reverse(l), ne)((x,y) => f(y,x))
  
  def sum2(ints: MyList[Int]): Int = 
    foldRight(ints, 0)((x,y) => x+y)
 
  def product2(ds: MyList[Double]): Double = 
    foldRight(ds, 1.0)(_*_)

  def length[A](l: MyList[A]): Int = 
    foldRight(l, 0)((_,acc) => acc+1)

  def drop[A](l: MyList[A], n: Int): MyList[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }
  }

  def dropWhile[A](l: MyList[A])(p: A => Boolean): MyList[A] = l match {
     case Cons(h, t) if p(h) => dropWhile(t)(p)
     case _ => l
  }

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def init[A](l: MyList[A]): MyList[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h,t) => Cons(h, init(t))
  }

  def apply[A](as: A*): MyList[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}


