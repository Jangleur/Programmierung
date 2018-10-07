object MyModule {

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def partial1[A, B, C](a: A, f: (A,B) => C): B => C =
    b => f(a, b)

  val lessThan = new Function2[Int, Int, Boolean] {
    def apply(a: Int, b: Int) = a < b
  }
  
  def testErgebnis(b: Boolean): String =
    if (b) "Bestanden"
    else  "Failed"
  

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = 
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n+1)
    
    loop(0)
  }
 
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n<=0) acc
      else go(n-1, n*acc)
  
    go(n, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "Der Absolutbetrag von %d ist %d"
    msg.format(x, abs(x))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "Der %s von %d ist %d"
    msg.format(name, n, f(n))
  } 

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}
