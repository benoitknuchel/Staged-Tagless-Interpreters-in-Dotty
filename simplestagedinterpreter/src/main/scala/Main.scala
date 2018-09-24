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

  //Environments bases, empty -> throw NoSuchElementException
  // A: Expr[Int] or Expr[Int => Int]
  def env0[A](s: String): A = throw new NoSuchElementException
  def fenv0[A](s: String): A = throw new NoSuchElementException

  def ext[A](env: (String => A), s: String, v: A): String => A = {
    y: String => if(s == y) v else env(y)
  }


  //The evaluator
  def eval1(e: Exp, env: String => Expr[Int], fenv: String => Expr[Int => Int]): Expr[Int] = e match {
    case int(x) => x.toExpr
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
      lazy val f: Int => Int = (x: Int) => ~eval1(e1, ext(env, s2, '(x)), ext(fenv, s1, '(f)))
      ~peval1(Program(tl, e), env, ext(fenv, s1, '(f))) //why also extend fenv ?
    }
  }


  def main(args: Array[String]): Unit = {

    val factorial = Program(List(Declaration
                            ("fact", "x", Ifz(Var("x"),
                                              int(1),
                                              Mul(Var("x"),
                                                  (App("fact", Sub(Var("x"), int(1)))))))
                          , Declaration("twoTimesFact", "y", Mul(int(2), App("fact", Var("y"))))),
                          App("twoTimesFact", int(5)))

    val res = peval1(factorial, env0, fenv0)
    println("=================")
    println("run : " + res.run)
    println("show : " + res.show)
    println("=================")

    val a = int(0)
    val b = Add(int(1), Mul(int(2), int(3)))
    val p = Ifz(a, int(2), b)
    val snd = eval1(p , env0, fenv0)
    println("=================")
    println("run : " + snd.run)
    println("show : " + snd.show)
    println("=================")

    val first = int(1: Int)
    val firstRes = eval1(first, env0, fenv0)
    println("=================")
    println("run : " + firstRes.run)
    println("show : " + firstRes.show)
    println("=================")



  }
}