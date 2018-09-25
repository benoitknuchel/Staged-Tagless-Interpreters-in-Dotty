import scala.quoted._

enum Exp {
  case int(x: Int)
  case Var(s: String)
  case App(s: String, e: Exp)
  case Add(e1: Exp, e2: Exp)
  case Sub(e1: Exp, e2: Exp)
  case Mul(e1: Exp, e2: Exp)
  case Div(e1: Exp, e2: Exp)
  case Ifz(e1: Exp, e2: Exp, e3: Exp)
}

enum Def {
  case Declaration(s1: String, s2: String, e: Exp)
}

enum Prog {
  case Program(list: List[Def], e: Exp)
}

object Main {
  implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make
  import Exp._
  import Def._
  import Prog._

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
              case (Some(x), Some(y)) => '{if(~y == 0) ~k(None) else ~k(Some('{~x / ~y}))}
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
        lazy val f: Int => Int = (x: Int) => ~eval(e1, ext(env, s2, '(x)), ext(fenv, s1, '(f)), k)
        ~peval_k(Program(tl, e), env, ext(fenv, s1, '(f)), k)
      }
    }

  def main(args: Array[String]): Unit = {

    //Some examples

    val first = Program(Nil, int(1: Int))
    val firstRes = peval(first, env0, fenv0)
    println("=================1")
    println("run : " + firstRes.run)
    println("show : " + firstRes.show)

    /*
    val a = int(0)
    val b = Add(int(1), Mul(int(2), int(3)))
    val p = Ifz(a, int(2), b)
    val snd = eval(b , env0, fenv0)
    println("=================2")
    println("run : " + snd.run) //ArrayIndexOutOfBoundsException, snd is already evaluated -> it fails when running (same when I try with factorial)
    println("show : " + snd.show)
    println("=================")


    val factorial = Program(List(Declaration
                            ("fact", "x", Ifz(Var("x"),
                                              int(1),
                                              Mul(Var("x"),
                                                  (App("fact", Sub(Var("x"), int(1)))))))
                          , Declaration("twoTimesFact", "y", Mul(int(2), App("fact", Var("y"))))),
                          App("twoTimesFact", int(5)))
    val res = peval(factorial, env0, fenv0)
    println("=================3")
    println("run : " + res.run)
    println("show : " + res.show)
    println("=================")*/

  }
}