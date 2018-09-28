
//z: index 0
//(s z) : index 1
//(s (s z)) : index 2
enum var2{
  case (VZ: exp, VS: var2)
}

enum exp{
  case V(v: var2)
  case B(b: Boolean)
  case L(e: exp)
  case A(e1: exp, e2: exp)
}
//env is the list of values bound to var
object Main {
  import var2._
  import exp._

  val env = Nil

  def lookup(x::env) = (v: var2) => v match {
    case VZ => x
    case v: VS => lookup(env)(v)
  }




  def main(args: Array[String]): Unit = {

  }

}
