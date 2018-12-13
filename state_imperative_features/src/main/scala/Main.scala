import scala.quoted._

trait Symantics {
  type repr[_, _]

  def num(x: Double): repr[Double, Double]
  def bool(b: Boolean): repr[Boolean, Boolean]

  def lam[A: Type, B: Type](f: repr[A, A] => repr[B, B]): repr[repr[A, A] => repr[B, B], A => B]
  def app[A, B](f: repr[repr[A, A] => repr[B, B], A => B], arg: repr[A, A]): repr[B, B]
  def fix[A: Type, B: Type](f: repr[repr[A, A] => repr[B, B], A => B] => repr[repr[A, A] => repr[B, B], A => B]): repr[repr[A, A] => repr[B, B], A => B]

  def neg(x: repr[Double, Double]): repr[Double, Double]
  def add(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]
  def mul(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]
  def div(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]
  def leq(x: repr[Double, Double], y: repr[Double, Double]): repr[Boolean, Boolean]
  def if_[A](cond: repr[Boolean, Boolean], e1: => repr[A, A], e2: => repr[A, A]): repr[A, A]
}

// type (’sv,’dv) repr = { ko: ’w. (’sv -> ’w) -> ’w }
abstract class repr[SV, DV] {
  //def ko[W](k: SV => W): W
  def ko[W](k: SV => W): SV => W
}

trait SymSI {
  type repr[_, _]
  type state
  //type states

  def num(x: Double): repr[Double, Double]
  def bool(b: Boolean): repr[Boolean, Boolean]

  def lam[A: Type, B: Type](f: repr[A, A] => repr[B, B]): repr[repr[A, A] => repr[B, B], A => B]
  def app[A, B](f: repr[repr[A, A] => repr[B, B], A => B], arg: repr[A, A]): repr[B, B]
  def fix[A: Type, B: Type](f: repr[repr[A, A] => repr[B, B], A => B] => repr[repr[A, A] => repr[B, B], A => B]): repr[repr[A, A] => repr[B, B], A => B]

  def neg(x: repr[Double, Double]): repr[Double, Double]
  def add(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]
  def mul(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]
  def div(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]
  def leq(x: repr[Double, Double], y: repr[Double, Double]): repr[Boolean, Boolean]
  def if_[A](cond: repr[Boolean, Boolean], e1: => repr[A, A], e2: => repr[A, A]): repr[A, A]

  //app with switched arguments
  def lapp[A: Type, B: Type](arg: repr[A, A], f: repr[A, A] => repr[B, B]): repr[B, B]
  def deref(): repr[state, state]
  def set(a: repr[state, state]): repr[state, state]

}

trait RCPS(val ST: SymSI) {

  type state = ST.state
  //type states //static version of the states, what does that mean ?

  def num[DV](x: Double): repr[Double, DV] = new repr[Double, DV] {
    def ko[W](k: Double => W): Double => W = s => k(x)
  }

  def bool[DV](b: Boolean): repr[Boolean, DV] = new repr[Boolean, DV] {
    def ko[W](k: Boolean => W): Boolean => W = s => k(b)
  }


  def lam[A, B](f: repr[A, A] => repr[B, B]): repr[repr[A, A] => repr[B, B], A => B] = new repr[repr[A, A] => repr[B, B], A => B] {
    def ko[W](k: (repr[A, A] => repr[B, B]) => W): (repr[A, A] => repr[B, B]) => W =
      s => k(f)
  }

  //let app e1 e2 = {ko = fun k -> e1.ko (fun f -> (f e2).ko k)}
  def app[A, B](f: repr[repr[A, A] => repr[B, B], A => B], arg: repr[A, A]): repr[B, B] = new repr[B, B] {
    def ko[W](k: B => W): B => W =
      f.ko(vf => vf(arg).ko(k))
  }

  def fix[A, B](f: repr[repr[A, A] => repr[B, B], A => B] => repr[repr[A, A] => repr[B, B], A => B]): repr[repr[A, A] => repr[B, B], A => B] = {
    def fx(fi: repr[repr[A, A] => repr[B, B], A => B] => repr[repr[A, A] => repr[B, B], A => B]): repr[A, A] => repr[B, B] =
      app(f(lam(fx(f))), _: repr[A, A])
    lam(fx(f))
  }


  def neg[DV](x: repr[Double, DV]): repr[Double, DV] = new repr[Double, DV] {
    def ko[W](k: Double => W): Double => W =
      x.ko((v: Double) => k(-v))
  }

  // let add e1 e2 = {ko = fun k -> e1.ko (fun v1 -> e2.ko (fun v2 -> k (v1 + v2)))}
  def add[DV](x: repr[Double, DV], y: repr[Double, DV]): repr[Double, DV] = new repr[Double, DV] {
    def ko[W](k: Double => W): Double => W =
      x.ko((v1: Double) => y.ko((v2: Double) => k(v1 + v2))(v1))
  }

  def mul[DV](x: repr[Double, DV], y: repr[Double, DV]): repr[Double, DV] = new repr[Double, DV] {
    def ko[W](k: Double => W): Double => W =
      x.ko((v1: Double) => y.ko((v2: Double) => k(v1 * v2))(v1))
  }

  def div[DV](x: repr[Double, DV], y: repr[Double, DV]): repr[Double, DV] = new repr[Double, DV] {
    def ko[W](k: Double => W): Double => W =
      x.ko((v1: Double) => y.ko((v2: Double) => k(v1 / v2))(v1))
  }

  def leq[DV](x: repr[Double, DV], y: repr[Double, DV]): repr[Boolean, DV] = new repr[Boolean, DV] {
    def ko[W](k: Boolean => W): Double => W =
      x.ko((v1: Double) => y.ko((v2: Double) => k(v1 <= v2))(v1 ))
  }

  //let if_ eb et ee =
  //  {ko = fun k -> eb.ko (fun vb -> if vb then (et ()).ko k else (ee ()).ko k)}
  def if_[A, DV](cond: repr[Boolean, DV], e1: => repr[A, DV], e2: => repr[A, DV]): repr[A, DV] = new repr[A, DV] {
    def ko[W](k: A => W): A => W =
      cond.ko((vb: Boolean) => if(vb) e1.ko(k) else e2.ko(k))
  }

  //let run x = x.ko (fun v -> v)
  //def run[SV, DV](x: repr[SV, DV]): SV = x.ko[SV]((v: SV) => v)

  //app with switched arguments (order of evaluation if also switched)
  def lapp[A: Type, B: Type](arg: repr[A, A], f: repr[A, A] => repr[B, B]): repr[B, B] = new repr[B, B] {
    def ko[W](k1: B => W): B => W =
      arg.ko[W]((v: A) => app(lam(f),
        new repr[A, A]{
          def ko[W2](k2: A => W2): A => W2 = (s: A) => k2(v)
        }).ko(k1))
  }

  //{ko = fun k s -> k s s}
  //let deref () = {ko = fun k s -> k s s}
  //def deref(a: Unit): repr[state, state] = new repr[state, state] {
  //  def ko[W](k: state => W): W = (s: state) => k.apply(s)
  //}

  //let set e = {ko = fun k -> e.ko (fun v s -> k s v)}
  //def set(e: repr[state, state]): repr[state, state] = new repr[state, state] {
  //  def ko[W](k: state => W): W = e.ko(v => k(v))
  //}

  //write “case e1 of x.e2 ” as "lapp e1 (fun x -> e2)",
  // "let x = e1 in e2" is "lapp e1 (\x -> e2)"
}


object Main {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make


  def main(args: Array[String]): Unit = {

  }
}


