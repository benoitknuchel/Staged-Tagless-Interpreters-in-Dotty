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
  // A: String
  // B: Expr[Int] or Expr[Int => Int]
  def env0[A, B](s: A): B = throw new NoSuchElementException
  def fenv0[A, B](s: A): B = throw new NoSuchElementException

  def ext[A, B](env: (A => B), s: A, v: B): A => B = {
    y: A => if(s == y) v else env(y)
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
    //I want to translate this in scala: Program (Declaration (s1,s2,e1)::tl,e) ->
    //                                      .<let rec f x = .~(eval2 e1 (ext env s2 .<x>.)
    //                                                      (ext fenv s1 .<f>.))
    //                                      in .~(peval2 (Program(tl,e)) env (ext fenv s1 .<f>.))>.
    case Program(Declaration(s1, s2, e1)::tl, e) => '{
      //s1 = e1(s2)
      //The problem here is to bind s1 to e1 (name to body) using the extension environment function 'ext'
      //Have to eval e1 and extend fenv by mapping s1 to e1 but they depend on one another
      //That's why it is not possible to pass '(f) to the ext funtion (compiler error : "could not find proxy for val f...")
      def f(x: Int): Int = ~eval1(e1, ext(env, s2, '(x)), ext(fenv, s1, '(f)))
      ~peval1(Program(tl, e), env, fenv)
    }
  }


  def main(args: Array[String]): Unit = {

    val programFactorial = Program(List(Declaration
                            ("fact", "x", Ifz(Var("x"),
                                              int(1),
                                              Mul(Var("x"),
                                                  (App("fact", Sub(Var("x"), int(1)))))))
                          ),
                          App("fact", int(10)))

    val res = peval1(programFactorial, env0, fenv0)
    println("=================")
    println("run : " + res.run)
    println("show : " + res.show)
    println("=================")

    /* These two examples work if I replace "ext(fenv, s1, '(f))" by "fenv" because we don't need to extend fenv...
    val a = int(0)
    val b = Add(int(1), Mul(int(2), int(3)))
    val p = Ifz(a, int(2), b)
    val snd = eval1(p , env0, fenv0)
    println("=================")
    println("run : " + snd.run)
    println("show : " + snd.show)
    println("=================")

    //works
    val first = int(1: Int)
    val firstRes = eval1(first, env0, fenv0)
    println("=================")
    println("run : " + firstRes.run)
    println("show : " + firstRes.show)
    println("=================")
    */


  }
}