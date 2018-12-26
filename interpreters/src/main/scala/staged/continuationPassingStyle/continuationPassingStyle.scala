package staged.continuationPassingStyle

import scala.quoted._
import staged.Exp._
import staged.Def._
import staged.Prog._

object continuationPassingStyle {

  import staged._

  //Environments bases, empty -> NoSuchElementException
  def env0[A](s: String): A = throw new NoSuchElementException
  def fenv0[A](s: String): A = throw new NoSuchElementException

  //[A]: Expr[Option[Int]] or Expr[Int => Option[Int]]
  def ext[A](env: (String => A), s: String, v: A): String => A = {
    y: String => if(s == y) v else env(y)
  }

  //The evaluator
  def eval[B](e: Exp, env: String => Expr[Int], fenv: String => Expr[Int => Int], k: Option[Expr[Int]] => Expr[B]): Expr[B] = e match {

    case int(x) => k(Some(x.toExpr))

    case Var(s) => k(Some(env(s)))

    case App(s, e) =>
      eval(e, env, fenv,
        (r: Option[Expr[Int]]) => r match {
          case Some(x) => k(Some('{(~fenv(s))(~x)}))
          case _ => k(None)
        }
      )

    case Add(e1, e2) =>
      eval(e1, env, fenv,
        (r: Option[Expr[Int]]) => {
          eval(e2, env, fenv,
            (s: Option[Expr[Int]]) => (r, s) match {
              case (Some(x), Some(y)) => k(Some('{~x + ~y}))
              case _ => k(None)
            }
          )
        }
      )

    case Sub(e1, e2) =>
      eval(e1, env, fenv,
        (r: Option[Expr[Int]]) => {
          eval(e2, env, fenv,
            (s: Option[Expr[Int]]) => (r, s) match {
              case (Some(x), Some(y)) => k(Some('{~x - ~y}))
              case _ => k(None)
            }
          )
        }
      )

    case Mul(e1, e2) =>
      eval(e1, env, fenv,
        (r: Option[Expr[Int]]) => {
          eval(e2, env, fenv,
            (s: Option[Expr[Int]]) => (r, s) match {
              case (Some(x), Some(y)) => k(Some('{~x * ~y}))
              case _ => k(None)
            }
          )
        }
      )

    case Div(e1, e2) =>
      eval(e1, env, fenv,
        (r: Option[Expr[Int]]) => {
          eval(e2, env, fenv,
            (s: Option[Expr[Int]]) => (r, s) match {
              case (Some(x), Some(y)) => '{if(~y == 0) throw new ArithmeticException else {val z = ~x / ~y; ~k(Some('{z}))}}
              case _ => k(None)
            }
          )
        }
      )

    case Ifz(e1, e2, e3) =>
      eval(e1, env, fenv,
        (r: Option[Expr[Int]]) => r match {
          case Some(x) => '{if(~x == 0) ~eval(e2, env, fenv, k) else ~eval(e3, env, fenv, k)}
          case _ => k(None)
        }
      )
  }

  def peval(p: Prog, env: String => Expr[Int], fenv: String => Expr[Int => Int]): Expr[Int] =
    peval_k(p, env, fenv,
      (x: Option[Expr[Int]]) => x match {
        case Some(x) => x
        case None => throw new IllegalArgumentException
      })

  def peval_k(p: Prog, env: String => Expr[Int], fenv: String => Expr[Int => Int], k: Option[Expr[Int]] => Expr[Int]): Expr[Int] =
    p match {
      case Program(Nil, e) => eval(e, env, fenv, k)

      //recursively eval each declaration
      case Program(Declaration(s1, s2, e1)::tl, e) => '{
        def f(x: Int): Int = ~eval(e1, ext(env, s2, '(x)), ext(fenv, s1, '(f)), k)
        ~peval_k(Program(tl, e), env, ext(fenv, s1, '(f)), k)
      }
    }

}

object Main {

  import continuationPassingStyle._

  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  def main(args: Array[String]): Unit = {

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
