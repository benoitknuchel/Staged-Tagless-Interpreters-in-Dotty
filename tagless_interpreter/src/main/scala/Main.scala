import scala.quoted._

//repr[_] is a wrapper (monad, ...)
trait Symantics[repr[_]] {
  def int(x: Int): repr[Int]
  def bool(b: Boolean): repr[Boolean]

  def lam[A, B](f: repr[A] => repr[B]): repr[A => B]
  def app[A, B](f: repr[A => B], arg: repr[A]): repr[B]

  def add(x: repr[Int], y: repr[Int]): repr[Int]
  def mul(x: repr[Int], y: repr[Int]): repr[Int]
  def leq(x: repr[Int], y: repr[Int]): repr[Boolean]
  def if_[A](cond: repr[Boolean], e1: repr[A], e2: repr[A]): repr[A]
}

object Main {
  implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make
  //No wrapper
  type Id[A] = A
  val eval: Symantics[Id] = new Symantics[Id] {
    override def int(x: Int): Int= x
    override def bool(b: Boolean): Boolean = b

    override def lam[A, B](f: A => B): A => B = f
    override def app[A, B](f: A => B, arg: A): B = f(arg)

    override def add(x: Int, y: Int): Int = x + y
    override def mul(x: Int, y: Int): Int = x * y
    override def leq(x: Int, y: Int): Boolean = x <= y
    override def if_[A](cond: Boolean, e1: A, e2: A): A = if (cond) e1 else e2
  }

  //staged interpreter
  type QuotedId[A] = Expr[A]
  val evalQuoted: Symantics[QuotedId] = new Symantics[QuotedId] {
    override def int(x: Int): QuotedId[Int] = x.toExpr
    override def bool(b: Boolean): QuotedId[Boolean] = b.toExpr

    override def lam[A, B](f: QuotedId[A] => QuotedId[B]): QuotedId[A => B] = '{ (x: A) => ~(f('(x))) }
    override def app[A, B](f: QuotedId[A => B], arg: QuotedId[A]): QuotedId[B] = '{ (~f)(~arg) }

    override def add(x: QuotedId[Int], y: QuotedId[Int]): QuotedId[Int] = '{ ~x + ~y }
    override def mul(x: QuotedId[Int], y: QuotedId[Int]): QuotedId[Int] = '{ ~x * ~y }
    override def leq(x: QuotedId[Int], y: QuotedId[Int]): QuotedId[Boolean] = '{ ~x <= ~y }
    override def if_[A](cond: QuotedId[Boolean], e1: QuotedId[A], e2: QuotedId[A]): QuotedId[A] = '{ if(~cond) ~e1 else ~e2 }
  }

  def main(args: Array[String]): Unit = {
    import eval._

    //(b=b)(true)
    /*
    val t1 = app(lam((b: Boolean) => bool(b)), bool(true))
    println("======================")
    println("res : " + t1)
    println("======================")*/

    //(x*x)(4)

    val t2 = app(lam((x: Int) => mul(x, x)), int(4))
    println("res : " + t2)
    println("======================")

    /*
    //(if(x <= 1) true else false)(1)
    val t3 = app(
      lam((x: Int) => if_(leq(x, int(1)), bool(true), bool(false))),
      int(1))
    println("res : " + t3.run)
    println("======================")

    //(if(n <= 2) n else n*(n-1))(5)
    val t4 = app(
      lam((n: Int) => if_(leq(n, int(2)), n, mul(n, add(n, -1)))),
        int(5)
    )
    println("res : " + t4)
    println("======================")*/

  }

}

/*
enum exp{
  case b(bv: Boolean)
  case lam(f: exp => exp)
  case app(f: exp => exp, x: exp)
}*/

/*
enum Symantics(repr: Any => Any){
  def int(x: Int): repr(Int)
  def bool(b: Boolean): repr(Boolean)

  def lam[A, B](f: repr(A)] => repr(B): rep(A => B)
  def app[A, B](f: repr(A => B), arg: repr(A)): repr(B)
  def fix[A](f: repr(A => A)): repr(A)

  def add(x: repr(Int), y: repr(Int)): repr(Int)
  def mul(x: repr(Int), y: repr(Int)): repr(Int)
  def leq(x: repr(Int), y: repr(Int)): repr(Boolean)
  def if_[A](cond: repr(Boolean), e1: repr(A), e2: repr(A)): repr(A)
}*/

/*
enum Symantics[repr[_]] {
  def int(x: Int): repr[Int]// = x
  def bool(b: Boolean): repr[Boolean]// = b

  def lam[A, B](f: repr[A] => repr[B]): repr[A => B]// = f
  def app[A, B](f: repr[A => B], arg: repr[A]): repr[B]// = f(arg)

  def add(x: repr[Int], y: repr[Int]): repr[Int]// = x + y
  def mul(x: repr[Int], y: repr[Int]): repr[Int]// = x * y
  def leq(x: repr[Int], y: repr[Int]): repr[Boolean]// = x <= y
  def if_[A](cond: repr[Boolean], e1: repr[A], e2: repr[A]): repr[A]// = if (cond) e1 else e2

  case intSymantics()
}*/

/*
implicit object intSymantics extends Symantics[Id] {
  override def int(x: Int): Id[Int] = x
  override def bool(b: Boolean): Id[Boolean] = b

  override def lam[A, B](f: Id[A] => Id[B]): Id[A => B] = f
  override def app[A, B](f: Id[A => B], arg: Id[A]): Id[B] = f(arg)

  override def add(x: Id[Int], y: Id[Int]): Id[Int] = x + y
  override def mul(x: Id[Int], y: Id[Int]): Id[Int] = x * y
  override def leq(x: Id[Int], y: Id[Int]): Id[Boolean] = x <= y
  override def if_[A](cond: Id[Boolean], e1: Id[A], e2: Id[A]): Id[A] = if (cond) e1 else e2
}*/

