package tagless.basic_evaluators.evalStaged

import tagless.Symantics

import scala.quoted._

//Staged tagless interpreter
object evalStaged extends Symantics {

  type repr[SV, DV] = Expr[DV]

  override def num(x: Double): Expr[Double] = x.toExpr

  override def bool(b: Boolean): Expr[Boolean] = b.toExpr

  //override def lam[A: Type, B: Type](f: Expr[A] => Expr[B]): Expr[A => B]
  override def lam[SA: Type, DA: Type, SB: Type, DB: Type](f: repr[SA, DA] => repr[SB, DB]): repr[repr[SA, DA] => repr[SB, DB], DA => DB] =
    '{ (x: DA) => ~(f('(x))) }

  //override def app[A, B](f: Expr[A => B], arg: Expr[A]): Expr[B]
  override def app[SA, DA, SB, DB](f: repr[repr[SA, DA] => repr[SB, DB], DA => DB], arg: repr[SA, DA]): repr[SB, DB] = f(arg) //'{ (~f)(~arg) }, use .asFunction()

  //override def fix[A: Type, B: Type](f: Expr[A => B] => Expr[A => B]): Expr[A => B]
  override def fix[SA: Type, DA: Type, SB: Type, DB: Type](f: repr[repr[SA, DA] => repr[SB, DB], DA => DB] =>
                                                              repr[repr[SA, DA] => repr[SB, DB], DA => DB]):
                                                              repr[repr[SA, DA] => repr[SB, DB], DA => DB] =
    '{ (~f(fix(f)))(_: DA) } //cannot stop recursion with the actual implementation of staging -> throw StackOverflowError

  override def neg(x: Expr[Double]): Expr[Double] = '{ -(~x) }

  override def add(x: Expr[Double], y: Expr[Double]): Expr[Double] = '{ ~x + ~y }

  override def mul(x: Expr[Double], y: Expr[Double]): Expr[Double] = '{ ~x * ~y }

  override def div(x: Expr[Double], y: Expr[Double]): Expr[Double] = '{ ~x / ~y }

  override def leq(x: Expr[Double], y: Expr[Double]): Expr[Boolean] = '{ ~x <= ~y }

  override def if_[A](cond: Expr[Boolean], e1: => Expr[A], e2: => Expr[A]): Expr[A] = '{ if(~cond) ~e1 else ~e2 }

}

object Main {

  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  def main(args: Array[String]): Unit = {

    import evalStaged._

    val t1 = app(lam((b: repr[Boolean, Boolean]) => b), bool(true))
    println("======================")
    println("(b=b)(true) : " + t1.show)
    println("result : " + t1.run)
    println("======================")

    val t2 = app(lam((x: repr[Double, Double]) => mul(x, x)), num(4))
    println("(x*x)(4) : " + t2.show)
    println("result : " + t2.run)
    println("======================")

    val t3 = app(
      lam((x: repr[Double, Double]) => if_(leq(x, num(1)), bool(true), bool(false))),
      num(1))
    println("(if(x <= 1) true else false)(1) : " + t3.show)
    println("result : " + t3.run)
    println("======================")

  }

}
