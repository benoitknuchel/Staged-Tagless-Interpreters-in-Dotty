package staged

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
