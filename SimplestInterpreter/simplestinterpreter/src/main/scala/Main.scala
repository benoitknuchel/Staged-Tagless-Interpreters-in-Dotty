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

  //Environments bases, empty -> throw exception
  def env0(s: String): Expr[Int] = throw new NoSuchElementException
  def fenv0(s: String): Expr[Int => Int] = throw new NoSuchElementException

  //Polymorphism yee
  //[A] shall be Expr[Int] or Expr[Int => Int]
  def ext[A](env: (String => A), s: String, v: A): (String => A) = {
    y: String => if(s == y) v else env(y)
  }

  //The evaluator/compiler
  def eval1(e: Exp, env: String => Expr[Int], fenv: String => Expr[Int => Int]): Expr[Int] = e match {
    case int(x) => x.toExpr //if '(x) compiler says unresolved symbols: value x when pickling Main.scala
    case Var(s) => env(s)
    case App(s, e) => '{ (~fenv(s))(~eval1(e, env, fenv)) }
    case Add(e1, e2) => '{ ~eval1(e1, env, fenv) + ~eval1(e2, env, fenv) }
    case Sub(e1, e2) => '{ ~eval1(e1, env, fenv) - ~eval1(e2, env, fenv) }
    case Mul(e1, e2) => '{ ~eval1(e1, env, fenv) * ~eval1(e2, env, fenv) }
    case Div(e1, e2) => '{ ~eval1(e1, env, fenv) / ~eval1(e2, env, fenv) }
    case Ifz(e1, e2, e3) => '{
      if(~eval1(e1, env, fenv) == 0) ~eval1(e2, env, fenv)
      else ~eval1(e3, env, fenv)
    }
  }

  def peval1(p: Prog, env: String => Expr[Int], fenv: String => Expr[Int => Int]): Expr[Int] = p match {
    case Program(Nil, e) => eval1(e, env, fenv)
    case Program(Declaration(s1, s2, e1)::tl, e) => '{
      def fun(x: Int): Int = ~eval1(e1, ext(env, s2, '(x)), ext(fenv, s1, '(fun)))
      ~peval1(Program(tl, e), env, ext(fenv, s1, '(fun)))
    }
  }



  def main(args: Array[String]): Unit = {
    /*
    val programFactorial = Program(List(Declaration
                            ("fact", "x", Ifz(Var("x"),
                                              int(1),
                                              Mul(Var("x"),
                                                  (App("fact", Sub(Var("x"), int(1)))))))
                          ),
                          App("fact", int(10)))

    val res = peval1(programFactorial, env0, fenv0)
    */

    val first = int(1: Int)
    val firstRes = eval1(first, env0, fenv0)


    println("=================")
    println("run : " + firstRes.run)
    println("show : " + firstRes.show)
    println("=================")

  }
}