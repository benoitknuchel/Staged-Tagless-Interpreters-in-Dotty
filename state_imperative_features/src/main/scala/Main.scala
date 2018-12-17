import scala.quoted._

trait Symantics {
  type repr[_, _]

  def num(x: Double): repr[Double, Double]
  def bool(b: Boolean): repr[Boolean, Boolean]

  def lam[A, B](f: repr[A, A] => repr[B, B]): repr[repr[A, A] => repr[B, B], A => B]
  def app[A, B](f: repr[repr[A, A] => repr[B, B], A => B], arg: repr[A, A]): repr[B, B]
  def fix[A, B](f: repr[repr[A, A] => repr[B, B], A => B] => repr[repr[A, A] => repr[B, B], A => B]): repr[repr[A, A] => repr[B, B], A => B]

  def neg(x: repr[Double, Double]): repr[Double, Double]
  def add(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]
  def mul(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]
  def div(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]
  def leq(x: repr[Double, Double], y: repr[Double, Double]): repr[Boolean, Boolean]
  def if_[A](cond: repr[Boolean, Boolean], e1: => repr[A, A], e2: => repr[A, A]): repr[A, A]
}

trait SymSI extends Symantics {

  abstract class repr[SV, DV] {
    def ko[W](k: (SV) => (State_ST => W)): State_ST => W
  }

  type State
  type State_ST

  def lapp[SA, DA, SB, DB](e2: repr[SA, DA], e1: repr[SA, DA] => repr[SB, DB]): repr[SB, DB]
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


  def lam[SA, DA, SB, DB](f: repr[SA, DA] => repr[SB, DB]): repr[repr[SA, DA] => repr[SB, DB], DA => DB] = new repr[repr[SA, DA] => repr[SB, DB], DA => DB] {
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

  def fix[SA, DA, SB, DB](f: repr[repr[SA, DA] => repr[SB, DB], DA => DB] => repr[repr[SA, DA] => repr[SB, DB], DA => DB]): repr[repr[SA, DA] => repr[SB, DB], DA => DB] = {
    def fx(fi: repr[repr[SA, DA] => repr[SB, DB], DA => DB] => repr[repr[SA, DA] => repr[SB, DB], DA => DB]): repr[SA, DA] => repr[SB, DB] =
      app(f(lam(fx(f))), _: repr[SA, DA])
    lam(fx(f))
  }


  def neg[DV](x: repr[Double, DV]): repr[Double, DV] = new repr[Double, DV] {
    def ko[W](k: Double => (State_ST => W)): (State_ST => W) =
      x.ko((v: Double) => k(-v))
  }

  def add[DV](x: repr[Double, DV], y: repr[Double, DV]): repr[Double, DV] = new repr[Double, DV] {
    def ko[W](k: Double => (State_ST => W)): (State_ST => W) =
      x.ko((v1: Double) => y.ko((v2: Double) => k(v1 + v2)))
  }

  def mul[DV](x: repr[Double, DV], y: repr[Double, DV]): repr[Double, DV] = new repr[Double, DV] {
    def ko[W](k: Double => (State_ST => W)): (State_ST => W) =
      x.ko((v1: Double) => y.ko((v2: Double) => k(v1 * v2)))
  }

  def div[DV](x: repr[Double, DV], y: repr[Double, DV]): repr[Double, DV] = new repr[Double, DV] {
    def ko[W](k: Double => (State_ST => W)): (State_ST => W) =
      x.ko((v1: Double) => y.ko((v2: Double) => k(v1 / v2)))
  }

  def leq[DV](x: repr[Double, DV], y: repr[Double, DV]): repr[Boolean, DV] = new repr[Boolean, DV] {
    def ko[W](k: Boolean => (State_ST => W)): (State_ST => W) =
      x.ko((v1: Double) => y.ko((v2: Double) => k(v1 <= v2)))
  }

  //let if_ eb et ee =
  //  {ko = fun k -> eb.ko (fun vb -> if vb then (et ()).ko k else (ee ()).ko k)}
  def if_[A, DV](cond: repr[Boolean, DV], e1: => repr[A, DV], e2: => repr[A, DV]): repr[A, DV] = new repr[A, DV] {
    def ko[W](k: A => (State_ST => W)): (State_ST => W) =
      cond.ko((vb: Boolean) => if(vb) e1.ko(k) else e2.ko(k))
  }


  def lapp[SA, DA, SB, DB](e2: repr[SA, DA], e1: repr[SA, DA] => repr[SB, DB]): repr[SB, DB] = new repr[SB, DB] {
    def ko[W](k: (SB) => (State_ST => W)): (State_ST => W) = {
      e2.ko((v: SA) => app[SA, DA, SB, DB](lam[SA, DA, SB, DB](e1), new repr[SA, DA] {
        def ko[W](k: (SA) => (State_ST => W)): (State_ST => W) = {
          k(v)
        }
      }).ko(k))
    }
  }

  def deref(): repr[State_ST, State] = new repr[State_ST, State] {
    def ko[W](k: State_ST => (State_ST => W)): (State_ST => W) = (s: State_ST) => k(s)(s)
  }

  def set(e: repr[State_ST, State]): repr[State_ST, State] = new repr[State_ST, State] {
    def ko[W](k: (State_ST) => (State_ST => W)): (State_ST => W) = e.ko(v => s => k(s)(v))
  }

  def getResult(x: repr[State_ST, State], init: State_ST) = x.ko(v => s => v)(init)
}


object Main {

  def main(args: Array[String]): Unit = {
    val e = new RCPS() { type State = Double; type State_ST= Double}

    import e._

    /*
        t1 corresponds to:

          var v0 = !state in
          state = 2;
          v0 + !state; 
    */
    val t1: repr[Double, Double] = lapp(deref(), v0 => lapp(set(num(2)), _ => add(v0, deref())))
    val t1res = getResult(t1, 100)
    println(t1res)

    val t2: repr[Double, Double] = add(set(num(2)), deref())
    val t2res = getResult(t2, 100)
    println(t2res)

    val pow =
      lam((x: repr[Double, Double]) => lapp(set(num(1)),
        _ => fix((self: repr[repr[Double, Double] => repr[Double, Double], Double => Double]) =>
          lam((n: repr[Double, Double]) => if_(leq(n, num(0)), deref(), lapp(set(mul(deref(), x)), _ =>
            app(self, add(n, neg(num(1))))))))))
    val pow7 = lam((x: repr[Double, Double]) => app(app(pow, x), num(7)))
    val pow27 = app(pow7, num(2))
    val pow27res = getResult(pow27, 0)
    println(pow27res)

    val fact =
        fix((self: repr[repr[Double, Double] => repr[Double, Double], Double => Double]) =>
          lam((n: repr[Double, Double]) => if_(leq(n, num(1)), deref(), lapp(set(mul(n, deref())), _ =>
            app(self, add(n, neg(num(1))))))))
    val fact5 = app(fact, num(5))
    val fact5res = getResult(fact5, 1)
    println(fact5res)
  }
}


