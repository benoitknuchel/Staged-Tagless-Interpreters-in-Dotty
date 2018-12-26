package tagless.basic_evaluators.partialEvaluator

import tagless.basic_evaluators.{eval, evalStaged}
import tagless.{Symantics, commonTypes}

import scala.quoted._

//Tagless partial evaluator using eval and evalQuoted
object partialEval extends Symantics {

  import commonTypes.StatDyn

  type repr[SV, DV] = StatDyn[SV, DV]

  def abstr[A, B](statDyn: StatDyn[A, B]): Expr[B] = statDyn._2

  def pdyn[A, B](x: Expr[B]): StatDyn[A, B] = (None, x)

  override def num(x: Double): StatDyn[Double, Double] = (Some(eval.eval.num(x)), evalStaged.evalStaged.num(x))

  override def bool(b: Boolean): StatDyn[Boolean, Boolean] = (Some(eval.eval.bool(b)), evalStaged.evalStaged.bool(b))

  //override def lam[A: Type, B: Type](f: StatDyn[A, A] => StatDyn[B, B]): StatDyn[StatDyn[A, A] => StatDyn[B, B], A => B]
  override def lam[SA: Type, DA: Type, SB: Type, DB: Type](f: repr[SA, DA] => repr[SB, DB]): repr[repr[SA, DA] => repr[SB, DB], DA => DB] =
    (Some(f), evalStaged.evalStaged.lam((x: Expr[DA]) => abstr(f(pdyn(x)))))

  //override def app[A, B](f: StatDyn[StatDyn[A, A] => StatDyn[B, B], A => B], arg: StatDyn[A, A]): StatDyn[B, B]
  override def app[SA, DA, SB, DB](f: repr[repr[SA, DA] => repr[SB, DB], DA => DB], arg: repr[SA, DA]): repr[SB, DB] = f._1 match {
    case Some(f) => f(arg) //f is (always?) static but we don't know about arg -> f: (Some(StatDyn[A, A] => StatDyn[B, B]), Expr[A => B])
    case _ => pdyn(evalStaged.evalStaged.app(abstr(f), abstr(arg))) //I don't think it'll ever come here
  }

  //override def fix[A: Type, B: Type](f: StatDyn[StatDyn[A, A] => StatDyn[B, B], A => B] => StatDyn[StatDyn[A, A] => StatDyn[B, B], A => B]): StatDyn[StatDyn[A, A] => StatDyn[B, B], A => B]
  override def fix[SA: Type, DA: Type, SB: Type, DB: Type](f: repr[repr[SA, DA] => repr[SB, DB], DA => DB] =>
                                                              repr[repr[SA, DA] => repr[SB, DB], DA => DB]):
                                                              repr[repr[SA, DA] => repr[SB, DB], DA => DB] = {
    def fdyn: Expr[DA => DB] = evalStaged.evalStaged.fix((x: Expr[DA => DB]) => abstr(f(pdyn(x))))
    lazy val self: StatDyn[SA, DA] => StatDyn[SB, DB] = {
      case e @ (_: Some[_], _) => app(f(lam(self)), e)
      case a => pdyn(evalStaged.evalStaged.app(fdyn, (abstr(a))))
    }
    (Some(self), fdyn)
  }

  override def neg(x: StatDyn[Double, Double]): StatDyn[Double, Double] = x._1 match {
    case Some(a) => (Some(eval.eval.neg(a)), evalStaged.evalStaged.neg(a.toExpr))
    case _ => pdyn(evalStaged.evalStaged.neg(x._2))
  }

  override def add(x: StatDyn[Double, Double], y: StatDyn[Double, Double]): StatDyn[Double, Double] = (x._1, y._1) match {
    case (Some(0), Some(a)) => y
    case (Some(a), Some(0)) => x
    case (Some(a), Some(b)) => num(eval.eval.add(a, b))
    case _ => pdyn(evalStaged.evalStaged.add(abstr(x), abstr(y)))
  }

  override def mul(x: StatDyn[Double, Double], y: StatDyn[Double, Double]): StatDyn[Double, Double] = (x._1, y._1) match {
    case (Some(0), Some(a)) => num(0)
    case (Some(a), Some(0)) => num(0)
    case (Some(1), Some(a)) => y
    case (Some(a), Some(1)) => x
    case _ => pdyn(evalStaged.evalStaged.mul(abstr(x), abstr(y)))
  }

  override def div(x: StatDyn[Double, Double], y: StatDyn[Double, Double]): StatDyn[Double, Double] = (x._1, y._1) match {
    case (Some(0), Some(a)) => num(0)
    case (Some(a), Some(0)) => throw new ArithmeticException
    case (Some(a), Some(1)) => x
    case _ => pdyn(evalStaged.evalStaged.div(abstr(x), abstr(y)))
  }

  override def leq(x: StatDyn[Double, Double], y: StatDyn[Double, Double]): StatDyn[Boolean, Boolean] = (x._1, y._1) match {
    case (Some(a), Some(b)) => bool(eval.eval.leq(a, b)) //if(a <= b) (Some(true), (true).toExpr) else (Some(false), (false.toExpr))
    case _ => pdyn(evalStaged.evalStaged.leq(abstr(x), abstr(y)))
  }

  override def if_[A](cond: StatDyn[Boolean, Boolean], e1: => StatDyn[A, A], e2: => StatDyn[A, A]): StatDyn[A, A] = cond._1 match {
    case Some(b) => if(b) e1 else e2
    case _ => pdyn(evalStaged.evalStaged.if_(abstr(cond), abstr(e1), abstr(e2)))
  }

}

object Main {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  def main(args: Array[String]): Unit = {

    import partialEval._

    val t1 = app(lam((b: repr[Boolean, Boolean]) => b), bool(true))
    println("======================")
    println("(b=b)(true) : " + t1._2.show)
    println("run dynamic part : " + t1._2.run)
    println("static part : " + t1._1)
    println("======================")

    val t2 = app(lam((x: repr[Double, Double]) => mul(x, x)), num(4))
    println("(x*x)(4) : " + t2._2.show)
    println("run dynamic part : " + t2._2.run)
    println("static part : " + t2._1)
    println("======================")

    val t3 = app(
      lam((x: repr[Double, Double]) => if_(leq(x, num(1)), bool(true), bool(false))),
      num(1))
    println("(if(x <= 1) true else false)(1) : " + t3._2.show)
    println("run dynamic part : " + t3._2.run)
    println("static part : " + t3._1)
    println("======================")

    val t4 = app(
      fix((fact: repr[repr[Double, Double] => repr[Double, Double], Double => Double]) =>
        (lam((n: repr[Double, Double]) => if_(leq(n, num(1)), n, mul(n, app(fact, add(n, neg(num(1))))))))
      ), num(10)
    )
    println("factorial(10) : " + t4._2.show)
    println("run dynamic part : " + t4._2.run)
    println("static part : " + t4._1)
    println("======================")

    val t5 = app(
      fix((rec: repr[repr[Double, Double] => repr[Double, Double], Double => Double]) =>
        (lam((n: repr[Double, Double]) => if_(leq(n, num(1)), div(num(1), n), add(div(num(1), mul(n, n)), app(rec, add(n, neg(num(1))))))))
      ), num(10)
    )
    println("Sum(1-10) 1/n^2 : " + t5._2.show)
    println("run dynamic part : " + t5._2.run)
    println("static part : " + t5._1)
    println("======================")

  }

}
