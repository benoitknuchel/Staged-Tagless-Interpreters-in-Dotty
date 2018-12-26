package tagless.basic_evaluators.eval

import tagless.{Symantics, commonTypes}

import scala.quoted._

//Tagless interpreter, no wrapper
object eval extends Symantics {

  import commonTypes.Id
  type repr[SV, DV] = Id[DV]

  override def num(x: Double): Id[Double] = x

  override def bool(b: Boolean): Id[Boolean] = b

  //override def lam[A: Type, B: Type](f: Id[A => B]): Id[A => B]
  override def lam[SA: Type, DA: Type, SB: Type, DB: Type](f: repr[SA, DA] => repr[SB, DB]): repr[repr[SA, DA] => repr[SB, DB], DA => DB] = f

  //override def app[A, B](f: Id[A => B], arg: Id[A]): Id[B]
  override def app[SA, DA, SB, DB](f: repr[repr[SA, DA] => repr[SB, DB], DA => DB], arg: repr[SA, DA]): repr[SB, DB] = f(arg)

  //override def fix[A: Type, B: Type](f: Id[A => B] => Id[A => B]): Id[A => B]
  override def fix[SA: Type, DA: Type, SB: Type, DB: Type](f: repr[repr[SA, DA] => repr[SB, DB], DA => DB] =>
                                                              repr[repr[SA, DA] => repr[SB, DB], DA => DB]):
                                                              repr[repr[SA, DA] => repr[SB, DB], DA => DB] =
    f(fix(f))(_: DA) //(x: A) => f(fix(f))(x)

  override def neg(x: Id[Double]): Id[Double] = -x

  override def add(x: Id[Double], y: Id[Double]): Id[Double] = x + y

  override def mul(x: Id[Double], y: Id[Double]): Id[Double] = x * y

  override def div(x: Id[Double], y: Id[Double]): Id[Double] = x / y

  override def leq(x: Id[Double], y: Id[Double]): Id[Boolean] = x <= y

  override def if_[A](cond: Id[Boolean], e1: => Id[A], e2: => Id[A]): Id[A] = if (cond) e1 else e2

}

object Main {

  def main(args: Array[String]): Unit = {

    import eval._

    val t1 = app(lam((b: repr[Boolean, Boolean]) => b), bool(true))
    println("======================")
    println("(b=b)(true) : " + t1)
    println("======================")

    val t2 = app(lam((x: repr[Double, Double]) => mul(x, x)), num(4))
    println("(x*x)(4) : " + t2)
    println("======================")

    val t3 = app(
      lam((x: repr[Double, Double]) => if_(leq(x, num(1)), bool(true), bool(false))),
      num(1))
    println("(if(x <= 1) true else false)(1) : " + t3)
    println("======================")

    val t4 = eval.app(
      eval.fix((fact: eval.repr[eval.repr[Double, Double] => eval.repr[Double, Double], Double => Double]) =>
        (eval.lam((n: eval.repr[Double, Double]) => eval.if_(eval.leq(n, eval.num(1)), n, eval.mul(n, eval.app(fact, eval.add(n, eval.neg(eval.num(1))))))))
      ), eval.num(5)
    )
    println("factorial(5) : " + t4)
    println("======================")

    val t5 = eval.app(
      eval.fix((rec: eval.repr[eval.repr[Double, Double] => eval.repr[Double, Double], Double => Double]) =>
        (eval.lam((n: eval.repr[Double, Double]) => eval.if_(eval.leq(n, eval.num(1)),
          eval.div(eval.num(1), n), eval.add(eval.div(eval.num(1), eval.mul(n, n)), eval.app(rec, eval.add(n, eval.neg(eval.num(1))))))))
      ), eval.num(10)
    )
    println("sum(1/n) 1 to 10 : " + t5)
    println("======================")

  }

}
