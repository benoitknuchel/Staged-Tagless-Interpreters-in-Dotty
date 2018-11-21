import scala.quoted._

trait Symantics {
  type repr[_, _]

  def num(x: Double): repr[Double, Double]
  def bool(b: Boolean): repr[Boolean, Boolean]

  def lam[A: Type, B: Type](f: repr[A, A] => repr[B, B]): repr[repr[A, A] => repr[B, B], A => B]
  def app[A, B](f: repr[repr[A, A] => repr[B, B], A => B], arg: repr[A, A]): repr[B, B]
  def fix[A: Type, B: Type](f: repr[repr[A, A] => repr[B, B], A => B] => repr[repr[A, A] => repr[B, B], A => B]): repr[repr[A, A] => repr[B, B], A => B]

  def neg(x: repr[Double, Double]): repr[Double, Double]
  def add(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]
  def mul(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]
  def div(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]
  def leq(x: repr[Double, Double], y: repr[Double, Double]): repr[Boolean, Boolean]
  def if_[A](cond: repr[Boolean, Boolean], e1: => repr[A, A], e2: => repr[A, A]): repr[A, A]
}

//Tagless interpreter, no wrapper
object eval extends Symantics {
  import Main.Id
  type repr[SV, DV] = Id[DV]

  override def num(x: Double): Id[Double] = x
  override def bool(b: Boolean): Id[Boolean] = b

  override def lam[A: Type, B: Type](f: Id[A => B]): Id[A => B] = f
  override def app[A, B](f: Id[A => B], arg: Id[A]): Id[B] = f(arg)
  override def fix[A: Type, B: Type](f: Id[A => B] => Id[A => B]): Id[A => B] = f(fix(f))(_: A) //(x: A) => f(fix(f))(x)

  override def neg(x: Id[Double]): Id[Double] = -x
  override def add(x: Id[Double], y: Id[Double]): Id[Double] = x + y
  override def mul(x: Id[Double], y: Id[Double]): Id[Double] = x * y
  override def div(x: Id[Double], y: Id[Double]): Id[Double] = x / y
  override def leq(x: Id[Double], y: Id[Double]): Id[Boolean] = x <= y
  override def if_[A](cond: Id[Boolean], e1: => Id[A], e2: => Id[A]): Id[A] = if (cond) e1 else e2

}

//Staged tagless interpreter
object evalQuoted extends Symantics {
  type repr[SV, DV] = Expr[DV]

  override def num(x: Double): Expr[Double] = x.toExpr
  override def bool(b: Boolean): Expr[Boolean] = b.toExpr

  override def lam[A: Type, B: Type](f: Expr[A] => Expr[B]): Expr[A => B] =  '{ (x: A) => ~(f('(x))) }
  override def app[A, B](f: Expr[A => B], arg: Expr[A]): Expr[B] = f(arg) //'{ (~f)(~arg) }, use .asFunction()
  override def fix[A: Type, B: Type](f: Expr[A => B] => Expr[A => B]): Expr[A => B] = '{ (~f(fix(f)))(_: A) } //cannot stop recursion -> throw StackOverflowError

  override def neg(x: Expr[Double]): Expr[Double] = '{ -(~x) }
  override def add(x: Expr[Double], y: Expr[Double]): Expr[Double] = '{ ~x + ~y }
  override def mul(x: Expr[Double], y: Expr[Double]): Expr[Double] = '{ ~x * ~y }
  override def div(x: Expr[Double], y: Expr[Double]): Expr[Double] = '{ ~x / ~y }
  override def leq(x: Expr[Double], y: Expr[Double]): Expr[Boolean] = '{ ~x <= ~y }
  override def if_[A](cond: Expr[Boolean], e1: => Expr[A], e2: => Expr[A]): Expr[A] = '{ if(~cond) ~e1 else ~e2 }

}


object Main {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  type Id[A] = A


  def main(args: Array[String]): Unit = {

    // TESTS
    //import eval._ //testing eval
    import evalQuoted._ //testing evalQuoted

    //(b=b)(true)
    val t1 = app(lam((b: repr[Boolean, Boolean]) => b), bool(true))
    println("======================")
    printRes(t1)

    //(x*x)(4)
    val t2 = app(lam((x: repr[Double, Double]) => mul(x, x)), num(4))
    printRes(t2)

    //(if(x <= 1) true else false)(1)
    val t3 = app(
      lam((x: repr[Double, Double]) => if_(leq(x, num(1)), bool(true), bool(false))),
      num(1))
    printRes(t3)

    // The next two examples cannot work with evalQuoted since the recursion ('fix') will go into an infinite loop
    println("/// t4 and t5 are evaluated with 'eval' and not 'evalQuoted' ///")
    println("======================")

    //factorial(5)
    val t4 = eval.app(
      eval.fix((fact: eval.repr[eval.repr[Double, Double] => eval.repr[Double, Double], Double => Double]) =>
        (eval.lam((n: eval.repr[Double, Double]) => eval.if_(eval.leq(n, eval.num(1)), n, eval.mul(n, eval.app(fact, eval.add(n, eval.neg(eval.num(1))))))))
      ), eval.num(10)
    )
    printRes(t4)

    //sum(1/n) 1 to 10
    val t5 = eval.app(
      eval.fix((rec: eval.repr[eval.repr[Double, Double] => eval.repr[Double, Double], Double => Double]) =>
        (eval.lam((n: eval.repr[Double, Double]) => eval.if_(eval.leq(n, eval.num(1)),
          eval.div(eval.num(1), n), eval.add(eval.div(eval.num(1), eval.mul(n, n)), eval.app(rec, eval.add(n, eval.neg(eval.num(1))))))))
      ), eval.num(10)
    )
    printRes(t5)

  }

  def printRes[A](res: Expr[A]): Unit ={
    println("show : " + res.show)
    println("res : " + res.run)
    println("======================")
  }

  def printRes[A](res: A): Unit = {
    println("res : " + res)
    println("======================")
  }

}
