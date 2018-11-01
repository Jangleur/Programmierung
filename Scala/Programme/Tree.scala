sealed trait MyTree[+A]
case class Leaf[A](value: A) extends MyTree[A]
case class Branch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {
  
  def size[A](t: MyTree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(r,l) => 1 + size(r) + size(l)
  }

  def maximum(t: MyTree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(r,l) => maximum(r) max maximum(l)
  }

  def depth[A](t: MyTree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(r,l) => 1 + (depth(r) max depth(l))
  }

  def map[A,B](t: MyTree[A])(f: A => B): MyTree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(r,l) => Branch(map(r)(f), map(l)(f))
  }

  def fold[A,B](t: MyTree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(r,l) => g(fold(r)(f)(g), fold(l)(f)(g))
  }

  def size2[A](t: MyTree[A]): Int = 
    fold(t)(x => 1)((x,y) => 1+x+y)
  
  def maximum2(t: MyTree[Int]): Int =
    fold(t)(x => x)((x,y) => x max y)

  def depth2[A](t: MyTree[A]): Int =
    fold(t)(x => 0)((x,y) => 1 + (x max y))
 
  def map2[A,B](t: MyTree[A])(f: A => B): MyTree[B] =
    fold(t)(x => Leaf(f(x)): MyTree[B])(Branch(_,_)) 

}
