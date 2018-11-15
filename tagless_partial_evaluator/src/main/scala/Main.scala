import scala.quoted._

//repr[_] is a wrapper
trait Symantics[repr[_]] {
  def num(x: Double): repr[Double]
  def bool(b: Boolean): repr[Boolean]

  def lam[A: Type, B: Type](f: repr[A] => repr[B]): repr[A => B]
  def app[A, B](f: repr[A => B], arg: repr[A]): repr[B]
  def fix[A: Type, B: Type](f: repr[A => B] => repr[A => B]): repr[A => B]

  def neg(x: repr[Double]): repr[Double]
  def add(x: repr[Double], y: repr[Double]): repr[Double]
  def mul(x: repr[Double], y: repr[Double]): repr[Double]
  def div(x: repr[Double], y: repr[Double]): repr[Double]
  def leq(x: repr[Double], y: repr[Double]): repr[Boolean]
  def if_[A](cond: repr[Boolean], e1: => repr[A], e2: => repr[A]): repr[A]
}

trait SymanticsD[repr[_, _]] {
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
import Main.Id
object eval extends Symantics[Id] {

  override def num(x: Double): Double = x
  override def bool(b: Boolean): Boolean = b

  override def lam[A: Type, B: Type](f: A => B): A => B = f
  override def app[A, B](f: A => B, arg: A): B = f(arg)
  override def fix[A: Type, B: Type](f: (A => B) => (A => B)): A => B = f(fix(f))(_: A) //(x: A) => f(fix(f))(x)

  override def neg(x: Double): Double = -x
  override def add(x: Double, y: Double): Double = x + y
  override def mul(x: Double, y: Double): Double = x * y
  override def div(x: Double, y: Double): Double = x / y
  override def leq(x: Double, y: Double): Boolean = x <= y
  override def if_[A](cond: Boolean, e1: => A, e2: => A): A = if (cond) e1 else e2

}

//Staged tagless interpreter
object evalQuoted extends Symantics[Expr] {

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

//Tagless partial evaluator
import Main.StatDyn
object partialEval extends SymanticsD[StatDyn] {

  import Main.StatDyn

  //To extract the dynamic part without needing to lift the static one
  def abstr[A, B](statDyn: StatDyn[A, B]): Expr[B] = statDyn._2
  def pdyn[A, B](x: Expr[B]): StatDyn[A, B] = (None, x)

  override def num(x: Double): StatDyn[Double, Double] = (Some(eval.num(x)), evalQuoted.num(x))
  override def bool(b: Boolean): StatDyn[Boolean, Boolean] = (Some(eval.bool(b)), evalQuoted.bool(b))

  override def lam[A: Type, B: Type](f: StatDyn[A, A] => StatDyn[B, B]): StatDyn[StatDyn[A, A] => StatDyn[B, B], A => B] = (Some(f), evalQuoted.lam((x: Expr[A]) => abstr(f(pdyn(x)))))
  override def app[A, B](f: StatDyn[StatDyn[A, A] => StatDyn[B, B], A => B], arg: StatDyn[A, A]): StatDyn[B, B] = f._1 match {
    case Some(f) => f(arg) //f is (always?) static but we don't know about arg -> f: (Some(StatDyn[A, A] => StatDyn[B, B]), Expr[A => B])
    case _ => pdyn(evalQuoted.app(abstr(f), abstr(arg))) //I don't think it'll ever come here
  }
  override def fix[A: Type, B: Type](f: StatDyn[StatDyn[A, A] => StatDyn[B, B], A => B] => StatDyn[StatDyn[A, A] => StatDyn[B, B], A => B]): StatDyn[StatDyn[A, A] => StatDyn[B, B], A => B] = {
    def fdyn: Expr[A => B] = evalQuoted.fix((x: Expr[A => B]) => abstr(f(pdyn(x))))
    lazy val self: StatDyn[A, A] => StatDyn[B, B] = {
      case e @ (_: Some[_], _) => app(f(lam(self)), e)
      case a => pdyn(evalQuoted.app(fdyn, (abstr(a))))
    }
    (Some(self), fdyn)
  }

  override def neg(x: StatDyn[Double, Double]): StatDyn[Double, Double] = x._1 match {
    case Some(a) => (Some(eval.neg(a)), evalQuoted.neg(a.toExpr))
    case _ => pdyn(evalQuoted.neg(x._2))
  }
  override def add(x: StatDyn[Double, Double], y: StatDyn[Double, Double]): StatDyn[Double, Double] = (x._1, y._1) match {
    case (Some(0), Some(a)) => y
    case (Some(a), Some(0)) => x
    case (Some(a), Some(b)) => num(eval.add(a, b))
    case _ => pdyn(evalQuoted.add(abstr(x), abstr(y)))
  }
  override def mul(x: StatDyn[Double, Double], y: StatDyn[Double, Double]): StatDyn[Double, Double] = (x._1, y._1) match {
    case (Some(0), Some(a)) => num(0)
    case (Some(a), Some(0)) => num(0)
    case (Some(1), Some(a)) => y
    case (Some(a), Some(1)) => x
    case _ => pdyn(evalQuoted.mul(abstr(x), abstr(y)))
  }
  override def div(x: StatDyn[Double, Double], y: StatDyn[Double, Double]): StatDyn[Double, Double] = (x._1, y._1) match {
    case (Some(0), Some(a)) => num(0)
    case (Some(a), Some(0)) => throw new ArithmeticException //num(Double.MaxValue)
    case (Some(a), Some(1)) => x
    case _ => pdyn(evalQuoted.div(abstr(x), abstr(y)))
  }
  override def leq(x: StatDyn[Double, Double], y: StatDyn[Double, Double]): StatDyn[Boolean, Boolean] = (x._1, y._1) match {
    case (Some(a), Some(b)) => bool(eval.leq(a, b)) //if(a <= b) (Some(true), (true).toExpr) else (Some(false), (false.toExpr))
    case _ => pdyn(evalQuoted.leq(abstr(x), abstr(y)))
  }
  override def if_[A](cond: StatDyn[Boolean, Boolean], e1: => StatDyn[A, A], e2: => StatDyn[A, A]): StatDyn[A, A] = cond._1 match {
    case Some(b) => if(b) e1 else e2
    case _ => pdyn(evalQuoted.if_(abstr(cond), abstr(e1), abstr(e2)))
  }

}



object Main {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  type Id[A] = A
  type StatDyn[A, B] = (Option[A], Expr[B])

  def main(args: Array[String]): Unit = {

    import partialEval._

    //(b=b)(true)
    val t1 = app(lam((b: StatDyn[Boolean, Boolean]) => b), bool(true))
    println("======================")
    printStatDyn(t1)

    //(x*x)(4)
    val t2 = app(lam((x: StatDyn[Double, Double]) => mul(x, x)), num(4))
    printStatDyn(t2)

    //(if(x <= 1) true else false)(1)
    val t3 = app(
      lam((x: StatDyn[Double, Double]) => if_(leq(x, num(1)), bool(true), bool(false))),
      num(1))
    printStatDyn(t3)

    //factorial(10)
    val t4 = app(
      fix((fact: StatDyn[StatDyn[Double, Double] => StatDyn[Double, Double], Double => Double]) =>
        (lam((n: StatDyn[Double, Double]) => if_(leq(n, num(1)), n, mul(n, app(fact, add(n, neg(num(1))))))))
      ), num(10)
    )
    printStatDyn(t4)

  //sum(1/n) 1 to 10
  val t5 = app(
    fix((rec: StatDyn[StatDyn[Double, Double] => StatDyn[Double, Double], Double => Double]) =>
      (lam((n: StatDyn[Double, Double]) => if_(leq(n, num(1)), div(num(1), n), add(div(num(1), mul(n, n)), app(rec, add(n, neg(num(1))))))))
    ), num(10)
  )
  println("Sum(1-10) 1/n^2 : ")
  println("pi^2 / 6 = " + Math.PI*Math.PI / 6)
  printStatDyn(t5)

  }

  def printStatDyn[A](s: StatDyn[A, A]): Unit ={
    println("(" + s._1 + ", " + s._2.show + ")")
    println("run_dynamic : " + s._2.run)
    println("======================")
  }

}

