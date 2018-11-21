//import scala.quoted._


// type (’c,’da,’db) darr -> type Darr[DA, DB] = Function1[DA, DB] //DA => DB

// type (’sv,’dv) repr = { ko: ’w. (’sv -> ’w) -> ’w }
abstract class Repr[SV, DV] {
  def ko[W](k: SV => W): W
}

trait RCN {
  def num[DV](x: Double): Repr[Double, DV] = new Repr[Double, DV] {
    def ko[W](k: Double => W): W = k(x)
  }

  def bool[DV](b: Boolean): Repr[Boolean, DV] = new Repr[Boolean, DV] {
    def ko[W](k: Boolean => W): W = k(b)
  }

  def lam[A, B, DV](f: Repr[A, DV] => Repr[B, DV]): Repr[Repr[A, DV] => Repr[B, DV], DV] = new Repr[Repr[A, DV] => Repr[B, DV], DV] {
    def ko[W](k: (Repr[A, DV] => Repr[B, DV]) => W): W =
      k(f)
  }

  def app[A, B, DV](f: Repr[Repr[A, DV] => Repr[B, DV], DV], arg: Repr[A, DV]): Repr[B, DV] = new Repr[B, DV] {
    def ko[W](k: B => W): W =
      f.ko((vf: Repr[A, DV] => Repr[B, DV]) => vf(arg).ko(k))
  }

  def fix[A, B, DV](f: Repr[Repr[A, DV] => Repr[B, DV], DV] => Repr[Repr[A, DV] => Repr[B, DV], DV]): Repr[Repr[A, DV] => Repr[B, DV], DV] = {
    def fx(fi: Repr[Repr[A, DV] => Repr[B, DV], DV] => Repr[Repr[A, DV] => Repr[B, DV], DV]): Repr[A, DV] => Repr[B, DV] =
      app(f(lam(fx(f))), _: Repr[A, DV])
    lam(fx(f))
  }

  def neg[DV](x: Repr[Double, DV]): Repr[Double, DV] = new Repr[Double, DV] {
    def ko[W](k: Double => W): W =
      x.ko((v: Double) => k(-v))
  }

  // let add e1 e2 = {ko = fun k -> e1.ko (fun v1 -> e2.ko (fun v2 -> k (v1 + v2)))}
  def add[DV](x: Repr[Double, DV], y: Repr[Double, DV]): Repr[Double, DV] = new Repr[Double, DV] {
    def ko[W](k: Double => W): W =
      x.ko((v1: Double) => y.ko((v2: Double) => k(v1 + v2)))
  }
  
  def mul[DV](x: Repr[Double, DV], y: Repr[Double, DV]): Repr[Double, DV] = new Repr[Double, DV] {
    def ko[W](k: Double => W): W =
      x.ko((v1: Double) => y.ko((v2: Double) => k(v1 * v2)))
  }
  
  def div[DV](x: Repr[Double, DV], y: Repr[Double, DV]): Repr[Double, DV] = new Repr[Double, DV] {
    def ko[W](k: Double => W): W =
      x.ko((v1: Double) => y.ko((v2: Double) => k(v1 / v2)))
  }
  
  def leq[DV](x: Repr[Double, DV], y: Repr[Double, DV]): Repr[Boolean, DV] = new Repr[Boolean, DV] {
    def ko[W](k: Boolean => W): W =
      x.ko((v1: Double) => y.ko((v2: Double) => k(v1 <= v2)))
  }

  def if_[A, DV](cond: Repr[Boolean, DV], e1: => Repr[A, DV], e2: => Repr[A, DV]): Repr[A, DV] = new Repr[A, DV] {
    def ko[W](k: A => W): W =
      cond.ko((vb: Boolean) => if(vb) e1.ko(k) else e2.ko(k))
  }

  //let run x = x.ko (fun v -> v)
  def run[SV, DV](x: Repr[SV, DV]): SV = x.ko[SV]((v: SV) => v)
}

object Main {

  object evaluator extends RCN 

  def main(args: Array[String]): Unit = {

    import evaluator._

    // 10
    val t00 = num(10)
    println("t00 : " + run(t00))

    // true
    val t0 = bool(true)
    println("t0 : " + run(t0))

    //(b=b)(true)
    val t1 = app(lam((b: Repr[Boolean, Boolean]) => b), bool(true))
    println("t1 : " + run(t1))

    /*
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


