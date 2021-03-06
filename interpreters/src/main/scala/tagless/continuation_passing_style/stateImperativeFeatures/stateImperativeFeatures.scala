package tagless.continuation_passing_style.stateImperativeFeatures

//Represents Symantics but with lam's and fix's generic types not staged
trait SymanticsTypeNotStaged {

  type repr[_, _]

  def num(x: Double): repr[Double, Double]

  def bool(b: Boolean): repr[Boolean, Boolean]

  //def lam[A: Type, B: Type](f: repr[A, A] => repr[B, B]): repr[repr[A, A] => repr[B, B], A => B]
  def lam[SA, DA, SB, DB](f: repr[SA, DA] => repr[SB, DB]): repr[repr[SA, DA] => repr[SB, DB], DA => DB]

  //def app[A, B](f: repr[repr[A, A] => repr[B, B], A => B], arg: repr[A, A]): repr[B, B]
  def app[SA, DA, SB, DB](f: repr[repr[SA, DA] => repr[SB, DB], DA => DB], arg: repr[SA, DA]): repr[SB, DB]

  //def fix[A: Type, B: Type](f: repr[repr[A, A] => repr[B, B], A => B] => repr[repr[A, A] => repr[B, B], A => B]): repr[repr[A, A] => repr[B, B], A => B]
  def fix[SA, DA, SB, DB](f: repr[repr[SA, DA] => repr[SB, DB], DA => DB] => repr[repr[SA, DA] => repr[SB, DB], DA => DB]): repr[repr[SA, DA] => repr[SB, DB], DA => DB]

  def neg(x: repr[Double, Double]): repr[Double, Double]

  def add(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]

  def mul(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]

  def div(x: repr[Double, Double], y: repr[Double, Double]): repr[Double, Double]

  def leq(x: repr[Double, Double], y: repr[Double, Double]): repr[Boolean, Boolean]

  def if_[A](cond: repr[Boolean, Boolean], e1: => repr[A, A], e2: => repr[A, A]): repr[A, A]

}

//Extends the basic Symantics by adding a feature : a state
trait SymSI extends SymanticsTypeNotStaged {

  abstract class repr[SV, DV] {
    def ko[W](k: (SV) => (State_ST => W)): State_ST => W
  }

  type State
  type State_ST

  //app with switched arguments, we first want to evaluate the argument
  def lapp[SA, DA, SB, DB](arg: repr[SA, DA], f: repr[SA, DA] => repr[SB, DB]): repr[SB, DB]

  //dereference the state -> returns the actual value of the state
  def deref(): repr[State_ST, State]

  //set the state to the given value and returns the old one
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
      e1.ko[W]((f: repr[SA, DA] => repr[SB, DB]) => f(e2).ko(k))
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

  //OCaml : let if_ eb et ee = {ko = fun k -> eb.ko (fun vb -> if vb then (et ()).ko k else (ee ()).ko k)}
  def if_[A, DV](cond: repr[Boolean, DV], e1: => repr[A, DV], e2: => repr[A, DV]): repr[A, DV] = new repr[A, DV] {
    def ko[W](k: A => (State_ST => W)): (State_ST => W) =
      cond.ko((vb: Boolean) => if(vb) e1.ko(k) else e2.ko(k))
  }

  def lapp[SA, DA, SB, DB](arg: repr[SA, DA], f: repr[SA, DA] => repr[SB, DB]): repr[SB, DB] = new repr[SB, DB] {
    def ko[W](k: (SB) => (State_ST => W)): (State_ST => W) = {
      arg.ko((v: SA) => app[SA, DA, SB, DB](lam[SA, DA, SB, DB](f), new repr[SA, DA] {
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

    val t1: repr[Double, Double] = lapp(deref(), (v0: repr[Double, Double]) => lapp(set(num(2)), (_: repr[Double, Double]) => add(v0, deref())))
    val t1res = getResult(t1, 100)
    println("===================")
    println("var v0 = !state in\n  state = 2;\n  v0 + !state;")
    println("init_state = 100")
    println(t1res)
    println("===================")

    val t2: repr[Double, Double] = add(set(num(2)), deref())
    val t2res = getResult(t2, 100)
    println("(state := 2) + !state")
    println("init_state = 100")
    println(t2res)
    println("===================")

    val pow =
      lam((x: repr[Double, Double]) => lapp(set(num(1)),
        (_: repr[Double, Double]) => fix((self: repr[repr[Double, Double] => repr[Double, Double], Double => Double]) =>
          lam((n: repr[Double, Double]) => if_(leq(n, num(0)), deref(), lapp(set(mul(deref(), x)), (_: repr[Double, Double]) =>
            app(self, add(n, neg(num(1))))))))))
    val pow7 = lam((x: repr[Double, Double]) => app(app(pow, x), num(7)))
    val pow27 = app(pow7, num(2))
    val pow27res = getResult(pow27, 0)
    println("imperative power: \n  fun x -> let _ = (state := 1) in\n    " +
      "fix (fun self -> fun n ->\n      if n<=0 then !state\n      " +
      "else let _ = state := !state * x in self(n-1))")
    println("2^7 : " + pow27res)
    println("===================")

    val fact =
      fix((self: repr[repr[Double, Double] => repr[Double, Double], Double => Double]) =>
        lam((n: repr[Double, Double]) => if_(leq(n, num(1)), deref(), lapp(set(mul(n, deref())), (_: repr[Double, Double]) =>
          app(self, add(n, neg(num(1))))))))
    val fact5 = app(fact, num(5))
    val fact5res = getResult(fact5, 1)
    println("factorial(5) : " + fact5res)
    println("===================")
  }
}

