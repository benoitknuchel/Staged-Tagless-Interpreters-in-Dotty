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

  //Environments bases, empty -> None
  def env0(s: String): Expr[Option[Int]] = '(None)
  def fenv0(s: String): Expr[Int => Option[Int]] = '((x: Int) => None)

  //[A]: Expr[Option[Int]] or Expr[Int => Option[Int]]
  def ext[A](env: (String => A), s: String, v: A): String => A = {
    y: String => if(s == y) v else env(y)
  }


  //The evaluator
  def eval(e: Exp, env: String => Expr[Option[Int]], fenv: String => Expr[Int => Option[Int]]): Expr[Option[Int]] = e match {
    case int(x) => '(Some(x)) //doesn't recognize x ("unresolved symbol"), compile if I write '(Some(1)) but still get an ArrayOutOfBoundsException
    case Var(s) => '(~env(s))
    case App(s, e) => '{
      ~eval(e, env, fenv) match {
        case Some(x) => (~fenv(s))(x)
        case _ => None
      }
    }
    case Add(e1, e2) => '{
      (~eval(e1, env, fenv), ~eval(e2, env, fenv)) match {
        case (Some(x), Some(y)) => Some(x+y)
        case _ => None
      }
    }
    case Sub(e1, e2) => '{
      (~eval(e1, env, fenv), ~eval(e2, env, fenv)) match {
        case (Some(x), Some(y)) => Some(x-y)
        case _ => None
      }
    }
    case Mul(e1, e2) => '{
      (~eval(e1, env, fenv), ~eval(e2, env, fenv)) match {
        case (Some(x), Some(y)) => Some(x*y)
        case _ => None
      }
    }
    case Div(e1, e2) => '{
      (~eval(e1, env, fenv), ~eval(e2, env, fenv)) match {
        case (Some(x), Some(y)) => if(y != 0) Some(x/y) else None
        case _ => None
      }
    }
    case Ifz(e1, e2, e3) => '{
      ~eval(e1, env, fenv) match {
        case Some(x) => if(x == 0) ~eval(e2, env, fenv) else ~eval(e3, env, fenv)
        case _ => None
      }
    }
  }

  def peval(p: Prog, env: String => Expr[Option[Int]], fenv: String => Expr[Int => Option[Int]]): Expr[Option[Int]] = p match {
    case Program(Nil, e) => eval(e, env, fenv)
    //recursively eval each declaration
    case Program(Declaration(s1, s2, e1)::tl, e) => '{
      lazy val f: Int => Option[Int] = (x: Int) => ~eval(e1, ext(env, s2, '(Some(x))), ext(fenv, s1, '(f)))
      ~peval(Program(tl, e), env, ext(fenv, s1, '(f)))
    }
  }


  def main(args: Array[String]): Unit = {

    //Some examples

    val first = Var("x")//int(1: Int)
    val firstRes = eval(first, env0, fenv0)
    println("=================")
    println("run : " + firstRes.run)
    println("show : " + firstRes.show)
    println("=================")


    val a = int(0)
    val b = Add(int(1), Mul(int(2), int(3)))
    val p = Ifz(a, int(2), b)
    val snd = eval(b , env0, fenv0)
    println("=================")
    println("run : " + snd.run)
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
    println("=================")
    println("The program comes here and then crash, 'factorial' is already evaluated")
    println("run : " + res.run)
    println("show : " + res.show)
    println("=================")

  }
}