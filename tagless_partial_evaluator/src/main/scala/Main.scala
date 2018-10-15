import scala.quoted._

//repr[_] is a wrapper
trait Symantics[repr[_]] {
  def int(x: Int): repr[Int]
  def bool(b: Boolean): repr[Boolean]

  def lam[A: Type, B: Type](f: repr[A] => repr[B]): repr[A => B]
  def app[A, B](f: repr[A => B], arg: repr[A]): repr[B]
  def fix[A, B](f: repr[A => B] => repr[A => B]): repr[A => B]

  def add(x: repr[Int], y: repr[Int]): repr[Int]
  def mul(x: repr[Int], y: repr[Int]): repr[Int]
  def leq(x: repr[Int], y: repr[Int]): repr[Boolean]
  def if_[A](cond: repr[Boolean], e1: repr[A], e2: repr[A]): repr[A]
}

trait SymanticsD[repr[_, _]] {
  def int(x: Int): repr[Int, Int]
  def bool(b: Boolean): repr[Boolean, Boolean]

  def lam[A: Type, B: Type](f: repr[A, A] => repr[B, B]): repr[repr[A, A] => repr[B, B], A => B]
  def app[A, B](f: repr[repr[A, A] => repr[B, B], A => B], arg: repr[A, A]): repr[B, B]
  //def fix[A, B](f: repr[repr[A, A] => repr[B, B], A => B] => repr[repr[A, A] => repr[B, B], A => B]): repr[repr[A, A] => repr[B, B], A => B]

  def add(x: repr[Int, Int], y: repr[Int, Int]): repr[Int, Int]
  def mul(x: repr[Int, Int], y: repr[Int, Int]): repr[Int, Int]
  def leq(x: repr[Int, Int], y: repr[Int, Int]): repr[Boolean, Boolean]
  def if_[A](cond: repr[Boolean, Boolean], e1: repr[A, A], e2: repr[A, A]): repr[A, A]
}

object Main {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  //Tagless interpreter, no wrapper
  type Id[A] = A
  val eval: Symantics[Id] = new Symantics[Id] {
    override def int(x: Int): Int = x
    override def bool(b: Boolean): Boolean = b

    override def lam[A: Type, B: Type](f: A => B): A => B = f
    override def app[A, B](f: A => B, arg: A): B = f(arg)
    override def fix[A, B](f: (A => B) => (A => B)): A => B = f(fix(f))(_)

    override def add(x: Int, y: Int): Int = x + y
    override def mul(x: Int, y: Int): Int = x * y
    override def leq(x: Int, y: Int): Boolean = x <= y
    override def if_[A](cond: Boolean, e1: A, e2: A): A = if (cond) e1 else e2
  }


  //Staged tagless interpreter
  val evalQuoted: Symantics[Expr] = new Symantics[Expr] {
    override def int(x: Int): Expr[Int] = x.toExpr
    override def bool(b: Boolean): Expr[Boolean] = b.toExpr

    override def lam[A: Type, B: Type](f: Expr[A] => Expr[B]): Expr[A => B] =  '{ (x: A) => ~f('(x)) } // = f.reflect() as said in "A Practical Unification of macros Multi-stage Programming and Macros" 6) Staged Lambdas
    override def app[A, B](f: Expr[A => B], arg: Expr[A]): Expr[B] = f(arg) //'{ (~f)(~arg) } //use .asFunction()
    override def fix[A, B](f: Expr[A => B] => Expr[A => B]): Expr[A => B] = f(fix(f))

    override def add(x: Expr[Int], y: Expr[Int]): Expr[Int] = '{ ~x + ~y }
    override def mul(x: Expr[Int], y: Expr[Int]): Expr[Int] = '{ ~x * ~y }
    override def leq(x: Expr[Int], y: Expr[Int]): Expr[Boolean] = '{ ~x <= ~y }
    override def if_[A](cond: Expr[Boolean], e1: Expr[A], e2: Expr[A]): Expr[A] = '{ if(~cond) ~e1 else ~e2 }
  }


  //Tagless partial evaluator
  //The StatDyn type represents is a pair with the left expression as an Option that contains either
  //a static expression (Id evaluator) or a dynnamic expression (Expr evaluator)
  type StatDyn[A, B] = (Option[A], Expr[B])
  val partialEval: SymanticsD[StatDyn] = new SymanticsD[StatDyn] {

    //To extract the dynamic part wihtout needing to lift the static one
    //Reify and reflect functions, reify=?asFunction
    def abstr[A, B](statDyn: StatDyn[A, B]): Expr[B] = statDyn._2
    def pdyn[A, B](x: Expr[B]): StatDyn[A, B] = (None, x)

    override def int(x: Int): StatDyn[Int, Int] = (Some(eval.int(x)), evalQuoted.int(x))
    override def bool(b: Boolean): StatDyn[Boolean, Boolean] = (Some(eval.bool(b)), evalQuoted.bool(b))

    override def lam[A: Type, B: Type](f: StatDyn[A, A] => StatDyn[B, B]): StatDyn[StatDyn[A, A] => StatDyn[B, B], A => B] = (Some(f), evalQuoted.lam((x: Expr[A]) => abstr(f(pdyn(x)))))
    override def app[A, B](f: StatDyn[StatDyn[A, A] => StatDyn[B, B], A => B], arg: StatDyn[A, A]): StatDyn[B, B] = f._1 match {
      case Some(f) => f(arg) //f is static but we don't know about arg -> f: (Some(StatDyn[A, A] => StatDyn[B, B]), Expr[A => B])
      case _ => pdyn(evalQuoted.app(abstr(f), abstr(arg)))
    }
    /* TODO : understand and finish 'fix'
    override def fix[A, B](f: StatDyn[StatDyn[A, A] => StatDyn[B, B], A => B] => StatDyn[StatDyn[A, A] => StatDyn[B, B], A => B]): StatDyn[StatDyn[A, A] => StatDyn[B, B], A => B] =
      f(pdyn(evalQuoted.fix((x: Expr[A]) => abstr(f(pdyn(x))))))*/

    override def add(x: StatDyn[Int, Int], y: StatDyn[Int, Int]): StatDyn[Int, Int] = (x._1, y._1) match {
      case (Some(0), Some(a)) => y
      case (Some(a), Some(0)) => x
      case (Some(a), Some(b)) => int(eval.add(a, b))
      case _ => pdyn(evalQuoted.add(abstr(x), abstr(y)))
    }
    override def mul(x: StatDyn[Int, Int], y: StatDyn[Int, Int]): StatDyn[Int, Int] = (x._1, y._1) match {
      case (Some(0), Some(a)) => (Some(0), 0.toExpr): StatDyn[Int, Int]
      case (Some(a), Some(0)) => (Some(0), 0.toExpr): StatDyn[Int, Int]
      case (Some(1), Some(a)) => y
      case (Some(a), Some(1)) => x
      case _ => pdyn(evalQuoted.mul(abstr(x), abstr(y)))
    }
    override def leq(x: StatDyn[Int, Int], y: StatDyn[Int, Int]): StatDyn[Boolean, Boolean] = (x._1, y._1) match {
      case (Some(a), Some(b)) => bool(eval.leq(a, b)) //if(a <= b) (Some(true), (true).toExpr) else (Some(false), (false.toExpr))
      case _ => pdyn(evalQuoted.leq(abstr(x), abstr(y)))
    }
    override def if_[A](cond: StatDyn[Boolean, Boolean], e1: StatDyn[A, A], e2: StatDyn[A, A]): StatDyn[A, A] = cond._1 match {
      case Some(b) => if(b) e1 else e2
      case _ => pdyn(evalQuoted.if_(abstr(cond), abstr(e1), abstr(e2)))
    }
  }


  def main(args: Array[String]): Unit = {

    import partialEval._

    //(b=b)(true)
    val t1 = app(lam((b: StatDyn[Boolean, Boolean]) => b), bool(true))
    println("======================")
    println("(" + t1._1 + ", " + t1._2.show + ")")
    println("run_dynamic : " + t1._2.run)
    println("======================")

    //(x*x)(4)
    val t2 = app(lam((x: StatDyn[Int, Int]) => mul(x, x)), int(4))
    println("(" + t2._1 + ", " + t2._2.show + ")")
    println("run_dynamic : " + t2._2.run)
    println("======================")

    //(if(x <= 1) true else false)(1)
    val t3 = app(
      lam((x: StatDyn[Int, Int]) => if_(leq(x, int(1)), bool(true), bool(false))),
      int(1))
    println("(" + t3._1 + ", " + t3._2.show + ")")
    println("run_dynamic : " + t3._2.run)
    println("======================")

  }

}

