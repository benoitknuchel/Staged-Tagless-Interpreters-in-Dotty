package tagless.continuation_passing_style.callByName

//OCaml : type (’sv,’dv) repr = { ko: ’w. (’sv -> ’w) -> ’w }
abstract class repr[SV, DV] {
  def ko[W](k: SV => W): W
}

//A call-by-name, continuation passing style, tagless interpreter
trait RCN {

  def num[DV](x: Double): repr[Double, DV] = new repr[Double, DV] {
    def ko[W](k: Double => W): W = k(x)
  }

  def bool[DV](b: Boolean): repr[Boolean, DV] = new repr[Boolean, DV] {
    def ko[W](k: Boolean => W): W = k(b)
  }

  def lam[A, B, DV](f: repr[A, DV] => repr[B, DV]): repr[repr[A, DV] => repr[B, DV], DV] = new repr[repr[A, DV] => repr[B, DV], DV] {
    def ko[W](k: (repr[A, DV] => repr[B, DV]) => W): W =
      k(f)
  }

  def app[A, B, DV](f: repr[repr[A, DV] => repr[B, DV], DV], arg: repr[A, DV]): repr[B, DV] = new repr[B, DV] {
    def ko[W](k: B => W): W =
      f.ko((vf: repr[A, DV] => repr[B, DV]) => vf(arg).ko(k))
  }

  def fix[A, B, DV](f: repr[repr[A, DV] => repr[B, DV], DV] => repr[repr[A, DV] => repr[B, DV], DV]): repr[repr[A, DV] => repr[B, DV], DV] = {
    def fx(fi: repr[repr[A, DV] => repr[B, DV], DV] => repr[repr[A, DV] => repr[B, DV], DV]): repr[A, DV] => repr[B, DV] =
      app(f(lam(fx(f))), _: repr[A, DV])
    lam(fx(f))
  }

  def neg[DV](x: repr[Double, DV]): repr[Double, DV] = new repr[Double, DV] {
    def ko[W](k: Double => W): W =
      x.ko((v: Double) => k(-v))
  }

  // OCaml: let add e1 e2 = {ko = fun k -> e1.ko (fun v1 -> e2.ko (fun v2 -> k (v1 + v2)))}
  def add[DV](x: repr[Double, DV], y: repr[Double, DV]): repr[Double, DV] = new repr[Double, DV] {
    def ko[W](k: Double => W): W =
      x.ko((v1: Double) => y.ko((v2: Double) => k(v1 + v2)))
  }

  def mul[DV](x: repr[Double, DV], y: repr[Double, DV]): repr[Double, DV] = new repr[Double, DV] {
    def ko[W](k: Double => W): W =
      x.ko((v1: Double) => y.ko((v2: Double) => k(v1 * v2)))
  }

  def div[DV](x: repr[Double, DV], y: repr[Double, DV]): repr[Double, DV] = new repr[Double, DV] {
    def ko[W](k: Double => W): W =
      x.ko((v1: Double) => y.ko((v2: Double) => k(v1 / v2)))
  }

  def leq[DV](x: repr[Double, DV], y: repr[Double, DV]): repr[Boolean, DV] = new repr[Boolean, DV] {
    def ko[W](k: Boolean => W): W =
      x.ko((v1: Double) => y.ko((v2: Double) => k(v1 <= v2)))
  }

  def if_[A, DV](cond: repr[Boolean, DV], e1: => repr[A, DV], e2: => repr[A, DV]): repr[A, DV] = new repr[A, DV] {
    def ko[W](k: A => W): W =
      cond.ko((vb: Boolean) => if(vb) e1.ko(k) else e2.ko(k))
  }

  //Ocaml: let run x = x.ko (fun v -> v)
  def run[SV, DV](x: repr[SV, DV]): SV = x.ko[SV]((v: SV) => v)

}

object Main {

  object evaluator extends RCN

  def main(args: Array[String]): Unit = {

    import evaluator._

    val t1 = app(lam((b: repr[Boolean, Boolean]) => b), bool(true))
    println("======================")
    println("(b=b)(true) : " + run(t1))

    val t2 = app(lam((x: repr[Double, Double]) => mul(x, x)), num(4))
    println("======================")
    println("(x*x)(4) : " + run(t2))

    val t3 = app(
      lam((x: repr[Double, Double]) => if_(leq(x, num(1)), bool(true), bool(false))),
      num(1))
    println("======================")
    println("(if(x <= 1) true else false)(1) : " + run(t3))

    val t4 = app(
      fix((fact: repr[repr[Double, Double] => repr[Double, Double], Double]) => //Double, Double => Double
        (lam((n: repr[Double, Double]) => if_(leq(n, num(1)), n, mul(n, app(fact, add(n, neg(num(1))))))))
      ), num(10)
    )
    println("======================")
    println("factorial(10) : " + run(t4))

    val t5 = app(
      fix((rec: repr[repr[Double, Double] => repr[Double, Double], Double]) =>
        (lam((n: repr[Double, Double]) => if_(leq(n, num(1)), div(num(1), n), add(div(num(1), mul(n, n)), app(rec, add(n, neg(num(1))))))))
      ), num(10)
    )
    println("======================")
    println("Sum(1-10) 1/n^2 : " + run(t5))

    val diverg = app(
      lam((x: repr[Double, Double]) => num(1)), app(fix((f: repr[repr[Double, Double] => repr[Double, Double], Double]) => f), num(2)))
    println("======================")
    println("(λx.1)(fix f.f)2 : " + run(diverg))
    println("which terminates under CBN but not CBV")
    println("======================")

  }

}