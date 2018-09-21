import scala.quoted._

enum Exp {
  case int(i: Int)
  case Var(x: String)
  case App(x: String, e: Exp)
  case Add(e1: Exp, e2: Exp)
  case Sub(e1: Exp, e2: Exp)
  case Mul(e1: Exp, e2: Exp)
  case Div(e1: Exp, e2: Exp)
  case Ifz(e1: Exp, e2: Exp, e3: Exp)
}

enum Def {
  case Declaration(x1: String, x2: String, e: Exp)
}

enum Prog {
  case Program(list: List[Def], e: Exp)
}

object Main {
  implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make
  import Exp._
  import Def._
  import Prog._

  //Environment
  def env0(x: Unit): Expr[Int] = throw new Exception
  def fenv0(x: Unit): Expr[Int => Int] = throw new Exception

  //Polymorphism yee
  def ext[A](env: (String => Expr[A]), x: String, v: Expr[A]): (String => Expr[A]) = {
    y: String => if(x == y) v else env(y)
  }

  def eval1(e: Exp, env: String => Expr[Int], fenv: String => Expr[Int => Int]): Expr[Int] = e match {
    case int(i) => '(i)
    case Var(x) => env(x)
    case App(x, e) => '{ (~fenv(x))(~eval1(e, env, fenv)) }
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
      '{ def f(x: String): Expr[Int => Int] = ~eval1(e1, ext(env, s2, '(x)), extf(fenv, s1, '(f)))
      ~peval1(Program(tl, e), env, extf(fenv, s1, f)) }
    }
  }



  def main(args: Array[String]): Unit = {
    val program = Program([Declaration
                            ("fact", "x", Ifz(Var "x",
                                              int 1,
                                              Mul(Var "x",
                                                  (App("fact", Sub(Var "x", int 1))))))
                          ],
                          App("fact", int 10))

    val res = peval1(program, env0, fenv0)
    println("=================")
    println("run : " + res.run)
    println("show : " + res.show)
    println("=================")

  }
}