import scala.quoted._
import partialEval._
import eval._
import evalQuoted._

object eval2 extends Symantics2 {
  import Main.Id
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

object evalQuoted2 extends Symantics2 {
  type repr[DV] = Expr[DV]

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

class CPSTransformer[Sym <: Symantics2  & Singleton](val S : Sym){
  import S._

  type W = Unit
  //type repr[SV, DV] = S.repr[SV, DV => W => W]

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

  //def run[A: Type](x: repr[(A => A) => A]): A = x(v => v)
  //def run[A: Type, W: Type](x: repr[A], f: repr[A] => W): W = f(x)

  //let run x = x.ko (fun v -> v)
  //def run[SV, DV](x: repr[SV, DV]): SV = x.ko[SV]((v: SV) => v)

}

object Main {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  type Id[A] = A
  type StatDyn[A, B] = (Option[A], Expr[B])

  def main(args: Array[String]): Unit = {
/*
    //Those two examples work
    val eval2CPS = new CPSTransformer(eval2)

    val t1 = eval2CPS.num[Double](10)
    val t1Res = t1(v => v)
    println("===================")
    println("t1 : " + t1Res)
    println("===================")

    val t2 = eval2CPS.app(eval2CPS.lam(x => x), eval2CPS.bool(true))
    val t2Res = t2(v => v)
    println("t2 : " + t2Res)
    println("===================")*/


    val quotedEvaluator2 = new CPSTransformer(evalQuoted2)

    val t1 : Expr[(Double => Double) => Double] = quotedEvaluator2.num[Double](10)
    val t1Res: Expr[Double] = t1('{ v => v }) //The compiler doesn't complain here, so the type of t1Res must be indeed Expr[Double]
    println("===================")
    println("t1 : " + t1Res) //But here if we run this we have : Expr(Expr(<pickled tasty>) <applied to> Expr(<pickled tasty>))
                            // => if we call t1Res.show (or run) we have a scala.MatchError
    println("===================")

    //Same error with t2
    val t2: Expr[(Boolean => Boolean) => Boolean] = quotedEvaluator2.app(quotedEvaluator2.lam[Boolean, Boolean, Boolean](x => x), quotedEvaluator2.bool(true))
    val t2Res: Expr[Boolean] = t2('{ v => v })
    println("t2 : " + t2Res)
    println("===================")

  }

}


