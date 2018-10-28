sealed trait MyTree[+A]
case object EmptyLeaf extends MyTree[Nothing]
case class Leaf[A](value: A) extends MyTree[A]
case class Branch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {
  def apply[A](as: A*): MyTree[A] = {
    if (as.isEmpty) EmptyLeaf
    else if ((as.tail).isEmpty) Leaf(as.head)
    else Branch(apply(as.head), apply(as.tail: _*))
  }
}
