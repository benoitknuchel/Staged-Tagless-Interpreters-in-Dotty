import scala.quoted._

// trait Symantics {
//   type repr[_, _]

//   def num(x: Double): repr[Double, Double]
//   def bool(b: Boolean): repr[Boolean, Boolean]

//   def lam[A: Type, B: Type](f: repr[A, A] => repr[B, B]): repr[repr[A, A] => repr[B, B], A => B]
//   def app[A, B](f: repr[repr[A, A] => repr[B, B], A => B], arg: repr[A, A]): repr[B, B]
//   def fix[A: Type, B: Type](f: repr[repr[A, A] => repr[B, B], A => B] => repr[repr[A, A] => repr[B, B], A => B]): repr[repr[A, A] => repr[B, B], A => B]

//   def neg[A: Type](x: repr[A, A]): repr[Double, Double]
//   def add(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]
//   def mul(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]
//   def div(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]
//   def leq(x: repr[Double, Double], y: repr[Double, Double]): repr[Boolean, Boolean]
//   def if_[A](cond: repr[Boolean, Boolean], e1: => repr[A, A], e2: => repr[A, A]): repr[A, A]
// }

trait SymSI /* extends Symantics */ {
  abstract class repr[SV, DV] {
    def ko[W](k: (SV) => (State_ST => W)): State_ST => W
  }

  type State
  type State_ST

  def lapp[SA: Type, DA: Type, SB: Type, DB: Type](e2: repr[SA, DA], e1: repr[SA, DA] => repr[SB, DB]): repr[SB, SB]
  def deref(): repr[State_ST, State]
  def set(a: repr[State_ST, State]): repr[State_ST, State]
}

class RCPS() extends SymSI {

  def num[DV](x: Double): repr[Double, DV] = new repr[Double, DV] {
    def ko[W](k: Double => (State_ST => W)): (State_ST => W) = k(x)
  }

  def bool[DV](b: Boolean): repr[Boolean, DV] = new repr[Boolean, DV] {
    def ko[W](k: (Boolean) => (State_ST => W)): State_ST => W = k(b)
  }

  def lam[SA, DA, SB, DB](f: repr[SA, DA] => repr[SB, DB]) = new repr[repr[SA, DA] => repr[SB, DB], DA => DB] {
    def ko[W](k: (repr[SA, DA] => repr[SB, DB]) => (State_ST => W)): State_ST => W = {
      k(f)
    }
  }
  
  def app[SA, DA, SB, DB](e1: repr[repr[SA, DA] => repr[SB, DB], DA => DB], 
                          e2: repr[SA, DA]): repr[SB, DB] = new repr[SB, DB] {
    def ko[W](k: (SB) => (State_ST => W)): State_ST => W = {
      e1.ko[W]((f: repr[SA, DA] => repr[SB, DB]) => f.apply(e2).ko(k))
    }
  }

  // def fix[A, B](f: repr[repr[A, A] => repr[B, B], A => B] => repr[repr[A, A] => repr[B, B], A => B]): repr[repr[A, A] => repr[B, B], A => B] = {
  //   def fx(fi: repr[repr[A, A] => repr[B, B], A => B] => repr[repr[A, A] => repr[B, B], A => B]): repr[A, A] => repr[B, B] =
  //     app(f(lam(fx(f))), _: repr[A, A])
  //   lam(fx(f))
  // }

  // def neg[DV](x: repr[Double, DV]): repr[Double, DV] = new repr[Double, DV] {
  //   def ko[W](k: Double => W): Double => W =
  //     x.ko((v: Double) => k(-v))
  // }

  def add[DV](x: repr[Double, DV], y: repr[Double, DV]): repr[Double, DV] = new repr[Double, DV] {
    def ko[W](k: Double => (State_ST => W)): (State_ST => W) =
      x.ko((v1: Double) => y.ko((v2: Double) => k(v1 + v2)))
  }

  // def mul[DV](x: repr[Double, DV], y: repr[Double, DV]): repr[Double, DV] = new repr[Double, DV] {
  //   def ko[W](k: Double => W): Double => W =
  //     x.ko((v1: Double) => y.ko((v2: Double) => k(v1 * v2))(v1))
  // }

  // def div[DV](x: repr[Double, DV], y: repr[Double, DV]): repr[Double, DV] = new repr[Double, DV] {
  //   def ko[W](k: Double => W): Double => W =
  //     x.ko((v1: Double) => y.ko((v2: Double) => k(v1 / v2))(v1))
  // }

  // def leq[DV](x: repr[Double, DV], y: repr[Double, DV]): repr[Boolean, DV] = new repr[Boolean, DV] {
  //   def ko[W](k: Boolean => W): Double => W =
  //     x.ko((v1: Double) => y.ko((v2: Double) => k(v1 <= v2))(v1 ))
  // }

  // //let if_ eb et ee =
  // //  {ko = fun k -> eb.ko (fun vb -> if vb then (et ()).ko k else (ee ()).ko k)}
  // def if_[A, DV](cond: repr[Boolean, DV], e1: => repr[A, DV], e2: => repr[A, DV]): repr[A, DV] = new repr[A, DV] {
  //   def ko[W](k: A => W): A => W =
  //     cond.ko((vb: Boolean) => if(vb) e1.ko(k) else e2.ko(k))
  // }

  //let run x = x.ko (fun v -> v)
  //def run[SV, DV](x: repr[SV, DV]): SV = x.ko[SV]((v: SV) => v)

  def lapp[SA: Type, DA: Type, SB: Type, DB: Type](e2: repr[SA, DA], e1: repr[SA, DA] => repr[SB, DB]): repr[SB, SB] = new repr[SB, SB] {
    def ko[W](k: SB => (State_ST => W)): (State_ST => W) = {
      e2.ko((v: SA) => app[SA, DA, SB, DB](lam[SA, DA, SB, DB](e1), new repr[SA, DA] {
        def ko[W](k: SA => (State_ST => W)): (State_ST => W) = {
          k(v)
        }
      }).ko(k))
    }
  }

  def deref(): repr[State_ST, State] = new repr[State_ST, State] {
    def ko[W](k: State_ST => (State_ST => W)): (State_ST => W) = (s: State_ST) => k(s)(s)
  }

  def set(e: repr[State_ST, State]): repr[State_ST, State] = new repr[State_ST, State] {
   def ko[W](k: State_ST => (State_ST => W)): (State_ST => W) = e.ko(v => s => k(s)(v))
  }

  def getResult(x: repr[State_ST, State], init: State_ST) = x.ko(v => s => v)(init)
}


object Main {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  def main(args: Array[String]): Unit = {
    val e = new RCPS() { type State = Double; type State_ST= Double}

    import e._
    /*  
        t1 corresponds to:

          var v0 = !state in
          state = 2;
          v0 + !state; 
    */

    val t1: repr[Double, Double] = lapp(deref(), v0 => lapp(set(num(2.0)), _ => add(v0, deref())))
    val t1res = getResult(t1, 100)
    println(t1res)
  }
}


