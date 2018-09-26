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
    case int(x) => '(Some(~x.toExpr): Option[Int])
    case Var(s) => '(~env(s))
    case App(s, e) => '{
      val a = ~eval(e, env, fenv)
      if(a.nonEmpty) (~fenv(s))(a.get)
      else None
      /*
      ~eval(e, env, fenv) match {
        case Some(x) => (~fenv(s))(x)
        case _ => None
      }*/
    }
    case Add(e1, e2) => '{
      val a = ~eval(e1, env, fenv)
      val b = ~eval(e2, env, fenv)
      if(a.nonEmpty && b.nonEmpty) Some(a.get + b.get)
      else None
      /*
      (~eval(e1, env, fenv), ~eval(e2, env, fenv)) match {
        case (Some(x), Some(y)) => Some(x+y): Option[Int]
        case _ => None: Option[Int]
      }*/
    }
    case Sub(e1, e2) => '{
      val a = ~eval(e1, env, fenv)
      val b = ~eval(e2, env, fenv)
      if(a.nonEmpty && b.nonEmpty) Some(a.get - b.get)
      else None
      /*
      (~eval(e1, env, fenv), ~eval(e2, env, fenv)) match {
        case (Some(x), Some(y)) => Some(x-y)
        case _ => None
      }*/
    }
    case Mul(e1, e2) => '{
      val a = ~eval(e1, env, fenv)
      val b = ~eval(e2, env, fenv)
      if(a.nonEmpty && b.nonEmpty) Some(a.get * b.get)
      else None
      /*
      (~eval(e1, env, fenv), ~eval(e2, env, fenv)) match {
        case (Some(x), Some(y)) => Some(x*y)
        case _ => None
      }*/
    }
    case Div(e1, e2) => '{
      val a = ~eval(e1, env, fenv)
      val b = ~eval(e2, env, fenv)
      if(a.nonEmpty && b.nonEmpty && b.get != 0) Some(a.get / b.get)
      else None
      /*
      (~eval(e1, env, fenv), ~eval(e2, env, fenv)) match {
        case (Some(x), Some(y)) => if(y != 0) Some(x/y) else None
        case _ => None
      }*/
    }
    case Ifz(e1, e2, e3) => '{
      val a = ~eval(e1, env, fenv)
      if(a.nonEmpty) {
        if (a.get == 0) ~eval(e2, env, fenv)
        else ~eval(e3, env, fenv)
      }else {
        None
      }
    /*
      ~eval(e1, env, fenv) match {
        case Some(x) => if(x == 0) ~eval(e2, env, fenv) else ~eval(e3, env, fenv)
        case _ => None
      }*/
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
    val first = Program(Nil, Div(int(4), int(2)))
    val firstRes = peval(first, env0, fenv0)
    println("=================1")
    println("run : " + firstRes.run)
    println("show : " + firstRes.show)


    val a = int(1)
    val b = Add(int(1), Mul(int(2), int(3)))
    val p = Program(Nil, Ifz(a, int(2), b))
    val snd = peval(p, env0, fenv0)
    println("=================2")
    println("run : " + snd.run)
    println("show : " + snd.show)



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
    println("=================")

  }
}
