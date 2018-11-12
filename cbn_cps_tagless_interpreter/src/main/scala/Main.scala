import scala.quoted._


object CBN_tagless {

  //type (’c,’sv,’dv) repr = {ko: ’w. (’sv -> ’w) -> ’w}
  trait Repr[SV, DV] {
    type W
    def ko(k: SV => W): W
  }


  def num(x: Double): Repr[Double, _] = new Repr[Double, _] {
    def ko(k: Double => W): W = k(x)
  }
  def bool(b: Boolean): Repr[Boolean, _] = new Repr[Boolean, _] {
    def ko(k: Boolean => W): W = k(b)
  }

  def lam[A, B](f: Repr[A, _] => Repr[B, _]): Repr[Repr[A, _] => Repr[B, _], _] = new Repr[Repr[A, _] => Repr[B, _], _] {
    def ko(k: (Repr[A, _] => Repr[B, _]) => W): W = k(f)
  }
  def app[A, B](f: Repr[Repr[A, _] => Repr[B, _], _], arg: Repr[A, _]): Repr[B, _] = new Repr[B, _] {
    def ko(k: B => f.W): f.W = f.ko((vf: Repr[A, _] => Repr[B, _]) => vf(arg).ko(k))
  }
  def fix[A, B](f: Repr[Repr[A, _] => Repr[B, _], _] => Repr[Repr[A, _] => Repr[B, _], _]): Repr[Repr[A, _] => Repr[B, _], _] = {
    def fx(fi: Repr[Repr[A, _] => Repr[B, _], _] => Repr[Repr[A, _] => Repr[B, _], _]): Repr[A, _] => Repr[B, _] = app(f(lam(fx(f))), _: Repr[A, _])
    lam(fx(f))
  }


  def neg(x: Repr[Double, _]): Repr[Double, _] = new Repr[Double, _] {
    def ko(k: Double => x.W): x.W = x.ko((v: Double) => k(-v)) //this compiles but I'm not sure about it
  }
  def add(x: Repr[Double, _], y: Repr[Double, _]): Repr[Double, _] = new Repr[Double, _] {
    def ko(k: Double => W): W = x.ko((v1: Double) => y.ko((v2: Double) => k(v1 + v2)))
  }
  def mul(x: Repr[Double, _], y: Repr[Double, _]): Repr[Double, _] = new Repr[Double, _] {
    def ko(k: Double => W): x.W = x.ko((v1: Double) => y.ko((v2: Double) => k(v1 * v2)))
  }
  def div(x: Repr[Double, _], y: Repr[Double, _]): Repr[Double, _] = new Repr[Double, _] {
    def ko(k: Double => W): x.W = x.ko((v1: Double) => y.ko((v2: Double) => k(v1 / v2)))
  }
  def leq(x: Repr[Double, _], y: Repr[Double, _]): Repr[Double, _] = new Repr[Boolean, _] {
    def ko(k: Boolean => W): W = x.ko((v1: Double) => y.ko((v2: Double) => k(v1 <= v2)))
  }


  def if_[A](cond: Repr[Boolean, _], e1: => Repr[A, _], e2: => Repr[A, _]): Repr[A, _] = new Repr[A, _] {
    def k0(k: A => W): W = cond.ko((vb: Boolean) => if(vb) (e1).ko(k) else (e2).ko(k))
  }

  
  def run[A](x: Repr[A, _]): A = x.ko((v: A) => v)

}



object Main {

  def main(args: Array[String]): Unit = {

    //import CBN_tagless._

    val t0 = num(10)
    println("t0 : " + run(t0))
    /*
    //(b=b)(true)
    val t1 = app(lam((b: Boolean) => b), bool(true))
    println("======================")
    println("t1 : " + run(t1))

    //(x*x)(4)
    val t2 = app(lam((x: Double) => mul(x, x)), num(4))
    println("======================")
    println("t2 : " + run(t2))

    //(if(x <= 1) true else false)(1)
    val t3 = app(
      lam((x: Double) => if_(leq(x, num(1)), bool(true), bool(false))),
      num(1))
    println("======================")
    println("t3 : " + run(t3))

    //factorial(10)
    val t4 = app(
      fix((fact: Double => Double) =>
        (lam((n: Double) => if_(leq(n, num(1)), n, mul(n, app(fact, add(n, neg(num(1))))))))
      ), num(10)
    )
    println("======================")
    println("t4 : " + run(t4))

    //sum(1/n) 1 to 10
    val t5 = app(
      fix((rec: Double => Double) =>
        (lam((n: Double) => if_(leq(n, num(1)), div(num(1), n), add(div(num(1), mul(n, n)), app(rec, add(n, neg(num(1))))))))
      ), num(10)
    )
    println("======================")
    println("Sum(1-10) 1/n^2 : ")
    println("pi^2 / 6 = " + Math.PI*Math.PI / 6)
    println("t5 : " + run(t5))
    println("======================")*/

  }

}


