package tagless.continuation_passing_style.callByValueTransformer

import tagless.commonTypes

import scala.quoted._

//Represent Symantics but with repr[_] and not repr[_, _]
trait Symantics2 {

  type repr[_]

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

object eval2 extends Symantics2 {

  import commonTypes.Id
  type repr[DV] = Id[DV]

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

object evalStaged2 extends Symantics2 {

  type repr[DV] = Expr[DV]

  override def num(x: Double): Expr[Double] = x.toExpr

  override def bool(b: Boolean): Expr[Boolean] = b.toExpr

  override def lam[A: Type, B: Type](f: Expr[A] => Expr[B]): Expr[A => B] =  '{ (x: A) => ~(f('(x))) }

  override def app[A, B](f: Expr[A => B], arg: Expr[A]): Expr[B] = f(arg) //equivalent to '{ (~f)(~arg) }, use .asFunction()

  override def fix[A: Type, B: Type](f: Expr[A => B] => Expr[A => B]): Expr[A => B] = '{ (~f(fix(f)))(_: A) } //cannot stop recursion -> throw StackOverflowError

  override def neg(x: Expr[Double]): Expr[Double] = '{ -(~x) }

  override def add(x: Expr[Double], y: Expr[Double]): Expr[Double] = '{ ~x + ~y }

  override def mul(x: Expr[Double], y: Expr[Double]): Expr[Double] = '{ ~x * ~y }

  override def div(x: Expr[Double], y: Expr[Double]): Expr[Double] = '{ ~x / ~y }

  override def leq(x: Expr[Double], y: Expr[Double]): Expr[Boolean] = '{ ~x <= ~y }

  override def if_[A](cond: Expr[Boolean], e1: => Expr[A], e2: => Expr[A]): Expr[A] = '{ if(~cond) ~e1 else ~e2 }

}

//A call-by-value continuation passing style transformer
//Takes an intepreter extending Symantics2 as argument and transforms it into a cps interpreter
class CPSTransformer[Sym <: Symantics2  & Singleton](val S : Sym){

  type W = Unit
  type repr[DV] = S.repr[DV]

  def num[W: Type](x: Double): repr[(Double => W) => W]  = S.lam(k => S.app(k, S.num(x)))

  def bool[W: Type](b: Boolean): repr[(Boolean => W) => W] = S.lam(k => S.app(k, S.bool(b)))

  def lam[A: Type, B: Type, W: Type](f: repr[(A => W) => W] => repr[(B => W) => W]): repr[((A => (B => W) => W) => W) => W] =
    S.lam(k1 => S.app(k1,
      S.lam(x => f(
        S.lam(k2 => S.app(k2, x))))))

  def app[A: Type, B: Type, W: Type](f: repr[((A => (B => W) => W) => W) => W], arg: repr[(A => W) => W]): repr[(B => W) => W] =
    S.lam(k =>
      S.app(f, S.lam(fv =>
        S.app(arg, S.lam(v =>
          S.app(S.app(fv, v), k))))))

  def fix[A: Type, B: Type, W: Type](f: repr[((A => (B => W) => W) => W) => W] => repr[((A => (B => W) => W) => W) => W]): repr[((A => (B => W) => W) => W) => W] =
    S.fix(f)

  def neg[W: Type](x: repr[(Double => W) => W]): repr[(Double => W) => W] =
    S.lam(k => S.app(x,
      S.lam(v => S.app(k, S.neg(v)))))

  def add[W: Type](x: repr[(Double => W) => W], y: repr[(Double => W) => W]): repr[(Double => W) => W] =
    S.lam(k =>
      S.app(x, S.lam(v1 =>
        S.app(y, S.lam(v2 =>
          S.app(k, S.add(v1, v2)))))))

  def mul[W: Type](x: repr[(Double => W) => W], y: repr[(Double => W) => W]): repr[(Double => W) => W] =
    S.lam(k =>
      S.app(x, S.lam(v1 =>
        S.app(y, S.lam(v2 =>
          S.app(k, S.mul(v1, v2)))))))

  def div[W: Type](x: repr[(Double => W) => W], y: repr[(Double => W) => W]): repr[(Double => W) => W] =
    S.lam(k =>
      S.app(x, S.lam(v1 =>
        S.app(y, S.lam(v2 =>
          S.app(k, S.div(v1, v2)))))))

  def leq[W: Type](x: repr[(Double => W) => W], y: repr[(Double => W) => W]): repr[(Boolean => W) => W] =
    S.lam(k =>
      S.app(x, S.lam(v1 =>
        S.app(y, S.lam(v2 =>
          S.app(k, S.leq(v1, v2)))))))

  def if_[A: Type, W: Type](cond: repr[(Boolean => W) => W], e1: => repr[(A => W) => W], e2: => repr[(A => W) => W]): repr[(A => W) => W] = {
    S.lam(k =>
      S.app(cond, S.lam(vCond =>
        S.if_(vCond,
          S.app(e1, k),
          S.app(e2, k)))))
  }

}


object Main {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  type Id[A] = A
  type StatDyn[A, B] = (Option[A], Expr[B])

  def main(args: Array[String]): Unit = {

    println("=== Basic Evaluator Transformed ===")

    val e2 = new CPSTransformer(eval2)

    val t1 = e2.num[Double](10)
    val t1Res = t1(v => v)
    println("===================")
    println("num(10) : " + t1Res)
    println("===================")

    val t2 = e2.app(e2.lam(x => x), e2.bool(true))
    val t2Res = t2(v => v)
    println("lam(x => x)(true) : " + t2Res)
    println("===================")

    val t3 = e2.app(e2.lam[Boolean, Double, Double](x =>
      e2.if_(x, e2.num(1), e2.num(2))), e2.bool(true))
    val t3Res = t3(v => v)
    println("lam(x => if(x) 1 else 2) (true) : " + t3Res)
    println("===================")

    val t4 = e2.app(
      e2.fix[Double, Double, Double](fact =>
        (e2.lam(n =>
          e2.if_(e2.leq(n, e2.num(1)),
            n,
            e2.mul(n, e2.app(fact, e2.add(n, e2.neg(e2.num(1))))))))
      ), e2.num(10)
    )
    val t4Res = t4(v => v)
    println("factorial(5) : " + t4Res)
    println("===================")

    val t5 = e2.app(
      e2.fix[Double, Double, Double](rec =>
        (e2.lam(n => e2.if_(e2.leq(n, e2.num(1)),
          e2.div(e2.num(1), n), e2.add(e2.div(e2.num(1), e2.mul(n, n)), e2.app(rec, e2.add(n, e2.neg(e2.num(1))))))))
      ), e2.num(10)
    )
    val t5Res = t5(v => v)
    println("sum(1/n^2) 1 to 10 : " + t5Res)
    println("===================")


    println()
    println("=== Staged evaluator transformed ===")

    val q2 = new CPSTransformer(evalStaged2)

    val t11= q2.num[Double](10)
    val t11Res= t11('{ v => v })
    println("===================")
    println("num(10) : " + t11Res.show)
    println("result : " + t11Res.run)
    println("===================")

    val t21 = q2.app(q2.lam(x => x), q2.bool(true))
    val t21Res = t21('{ v => v })
    println("lam(x => x)(true) : " + t21Res.show)
    println("result : " + t21Res.run)
    println("===================")

    val t31 = q2.app(q2.lam[Boolean, Double, Double](x =>
      q2.if_(x, q2.num(1), q2.num(2))), q2.bool(true))
    val t31Res = t31('{ v => v})
    println("lam(x => if(x) 1 else 2) (true) : " + t31Res.show)
    println("result : " + t31Res.run)
    println("===================")

  }
}