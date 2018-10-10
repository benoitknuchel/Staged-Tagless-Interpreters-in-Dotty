// Needed to run or show quotes

import org.junit.Test
import org.junit.Assert._

class Test1 {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  @Test def t1(): Unit = {
    val code = '(println("foo"))
    assertEquals("scala.Predef.println(\"foo\")", code.show)
  }
}
