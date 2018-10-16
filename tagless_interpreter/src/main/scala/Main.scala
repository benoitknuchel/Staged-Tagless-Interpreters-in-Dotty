import scala.quoted._

//repr[_] is a wrapper
trait Symantics[repr[_]] {
  def int(x: Int): repr[Int]
  def bool(b: Boolean): repr[Boolean]

  def lam[A: Type, B: Type](f: repr[A] => repr[B]): repr[A => B]
  def app[A, B](f: repr[A => B], arg: repr[A]): repr[B]
  def fix[A](f: repr[A => A] => repr[A => A]): repr[A => A]

  def add(x: repr[Int], y: repr[Int]): repr[Int]
  def mul(x: repr[Int], y: repr[Int]): repr[Int]
  def leq(x: repr[Int], y: repr[Int]): repr[Boolean]
  def if_[A](cond: repr[Boolean], e1: => repr[A], e2: => repr[A]): repr[A]
}

object Main {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  //Tagless interpreter, no wrapper
  type Id[A] = A
  val eval: Symantics[Id] = new Symantics[Id] {
    override def int(x: Int): Int= x
    override def bool(b: Boolean): Boolean = b

    override def lam[A: Type, B: Type](f: A => B): A => B = f
    override def app[A, B](f: A => B, arg: A): B = f(arg)
    override def fix[A](f: (A => A) => (A => A)): A => A = f(fix(f))(_: A) //(x: A) => f(fix(f))(x)

    override def add(x: Int, y: Int): Int = x + y
    override def mul(x: Int, y: Int): Int = x * y
    override def leq(x: Int, y: Int): Boolean = x <= y
    override def if_[A](cond: Boolean, e1: => A, e2: => A): A = if (cond) e1 else e2
  }


  //Staged tagless interpreter
  val evalQuoted: Symantics[Expr] = new Symantics[Expr] {
    override def int(x: Int): Expr[Int] = x.toExpr
    override def bool(b: Boolean): Expr[Boolean] = b.toExpr

    override def lam[A: Type, B: Type](f: Expr[A] => Expr[B]): Expr[A => B] =  '{ (x: A) => ~(f('(x))) } // = f.reflect() ?? as said in "A Practical Unification of macros Multi-stage Programming and Macros 6) Staged Lambdas"
    override def app[A, B](f: Expr[A => B], arg: Expr[A]): Expr[B] = f(arg) //'{ (~f)(~arg) }, use .asFunction()
    override def fix[A](f: Expr[A => A] => Expr[A => A]): Expr[A => A] = f(fix(f)) //'{ ~f(fix(f))(_: A) }

    override def add(x: Expr[Int], y: Expr[Int]): Expr[Int] = '{ ~x + ~y }
    override def mul(x: Expr[Int], y: Expr[Int]): Expr[Int] = '{ ~x * ~y }
    override def leq(x: Expr[Int], y: Expr[Int]): Expr[Boolean] = '{ ~x <= ~y }
    //e1 and e2 must be CBN because of 'fix' : if either e1 or e2 is a recursion then' it'll be evaluated immediately and so on -> infinite loop
    override def if_[A](cond: Expr[Boolean], e1: => Expr[A], e2: => Expr[A]): Expr[A] = '{ if(~cond) ~e1 else ~e2 }
  }


  def main(args: Array[String]): Unit = {

    // TEST STAGED
    import evalQuoted._

    //(b=b)(true)
    val t1 = app(lam((b: Expr[Boolean]) => b), bool(true))
    println("======================")
    println("show : " + t1.show)
    println("res : " + t1.run)
    println("======================")

    //(x*x)(4)
    val t2 = app(lam((x: Expr[Int]) => mul(x, x)), int(4))
    println("show : " + t2.show)
    println("res : " + t2.run)
    println("======================")

    //(if(x <= 1) true else false)(1)
    val t3 = app(
      lam((x: Expr[Int]) => if_(leq(x, int(1)), bool(true), bool(false))),
      int(1))
    println("show : " + t3.show)
    println("res : " + t3.run)
    println("======================")

    //factorial(5),
    //TODO: make it work, fix: Expr[A => B] => Expr[A => B]
    //val t4 = app(fix((f: Expr[Int => Int]) => '{ (n: Int) => if_(leq(n.toExpr, int(1)), n.toExpr, mul(f(add(n.toExpr, int(-1))), n.toExpr)) }), int(5))
    //val t4 = app(fix((f: Int => Int) => (n: Int) => if(leq(n, int(1))) n else mul(f(add(n, -1)), n)), int(5))
    //println("show : " + t4.show)
    //println("res : " + t4.run)
    //println("======================")

    ////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////
    /**
    //TEST ID

    import eval._

    //(b=b)(true)
    val t1 = app(lam((b: Boolean) => b), bool(true))
    println("======================")
    println("res : " + t1)
    println("======================")

    //(x*x)(4)
    val t2 = app(lam((x: Int) => mul(x, x)), int(4))
    println("res : " + t2)
    println("======================")

    //(if(x <= 1) 0 else (x-1)*(3*4))(2)
    val t3 = app(
      lam((x: Int) => if_(leq(x, int(1)), int(0), mul(add(x, -1), mul(3, 4)))),
      int(2))
    println("res : " + t3)
    println("======================")

    //factorial(5)
    val t4 = app(fix((f: Int => Int) => (n: Int) => if_(leq(n, int(1)), n, mul(f(add(n, -1)), n))), int(5))
    println("res : " + t4)
    println("======================")
    */

  }

}
