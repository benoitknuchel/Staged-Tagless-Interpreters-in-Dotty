package staged.stagedInterpreter

import scala.quoted._
import staged.Exp._
import staged.Def._
import staged.Prog._

object stagedInterpreter {

  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  import staged._

  //Environments bases, empty -> None
  def env0(s: String): Expr[Option[Int]] = '(None)
  def fenv0(s: String): Expr[Int => Option[Int]] = '((x: Int) => None)

  //[A]: Expr[Option[Int]] or Expr[Int => Option[Int]]
  def ext[A](env: (String => A), s: String, v: A): String => A = {
    y: String => if(s == y) v else env(y)
  }

  //The evaluator
  def eval(e: Exp, env: String => Expr[Option[Int]], fenv: String => Expr[Int => Option[Int]]): Expr[Option[Int]] = e match {
    case int(x) => '(Some(~x.toExpr))

    case Var(s) => '(~env(s))

    case App(s, e) => '{
      ~eval(e, env, fenv) match {
        case Some(x) => (~fenv(s))(x) : Option[Int]
        case _ => None : Option[Int]
      }
    }

    case Add(e1, e2) => '{
      (~eval(e1, env, fenv), ~eval(e2, env, fenv)) match {
        case (Some(x), Some(y)) => Some(x+y) : Option[Int]
        case _ => None : Option[Int]
      }
    }

    case Sub(e1, e2) => '{
      (~eval(e1, env, fenv), ~eval(e2, env, fenv)) match {
        case (Some(x), Some(y)) => Some(x-y) : Option[Int]
        case _ => None : Option[Int]
      }
    }

    case Mul(e1, e2) => '{
      (~eval(e1, env, fenv), ~eval(e2, env, fenv)) match {
        case (Some(x), Some(y)) => Some(x*y) : Option[Int]
        case _ => None : Option[Int]
      }
    }

    case Div(e1, e2) => '{
      (~eval(e1, env, fenv), ~eval(e2, env, fenv)) match {
        case (Some(x), Some(y)) => if(y != 0) Some(x/y) : Option[Int] else None : Option[Int]
        case _ => None : Option[Int]
      }
    }

    case Ifz(e1, e2, e3) => '{
      ~eval(e1, env, fenv) match {
        case Some(x) => if(x == 0) ~eval(e2, env, fenv) : Option[Int] else ~eval(e3, env, fenv) : Option[Int]
        case _ => None : Option[Int]
      }
    }

  }

  def peval(p: Prog, env: String => Expr[Option[Int]], fenv: String => Expr[Int => Option[Int]]): Expr[Option[Int]] = p match {
    case Program(Nil, e) => eval(e, env, fenv)

    //recursively eval each declaration
    case Program(Declaration(s1, s2, e1)::tl, e) => '{
      def f(x: Int): Option[Int] = ~eval(e1, ext(env, s2, '(Some(x))), ext(fenv, s1, '(f)))
      ~peval(Program(tl, e), env, ext(fenv, s1, '(f)))
    }

  }

}

object Main {

  import stagedInterpreter._

  def main(args: Array[String]): Unit = {

    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

    val t1 = Program(Nil, Div(int(10), int(2)))
    val t1Res = peval(t1, env0, fenv0)
    println("=================")
    println("int(10) / int(2) : " + t1Res.show)
    println("run : " + t1Res.run)


    val isZero = int(1)
    val e1 = int(2)
    val e2 = Add(int(1), Mul(int(2), int(3)))
    val t2 = Program(Nil, Ifz(isZero, e1, e2))
    val t2Res = peval(t2 , env0, fenv0)
    println("=================")
    println("(ifz(x) 2 else 1+2*3)(1) : " + t2Res.show)
    println("run : " + t2Res.run)


    val factorial = Program(List(Declaration
    ("fact", "x", Ifz(Var("x"),
      int(1),
      Mul(Var("x"),
        (App("fact", Sub(Var("x"), int(1)))))))
      , Declaration("twoTimesFact", "y", Mul(int(2), App("fact", Var("y"))))),
      App("twoTimesFact", int(5)))
    val res = peval(factorial, env0, fenv0)
    println("=================")
    println("2*factorial(5) : " + res.show)
    println("run : " + res.run)
    println("=================")

  }

}