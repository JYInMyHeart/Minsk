import eval.Compilation
import parser.SyntaxTree
import symbol.VariableSymbol

import scala.collection.mutable
import scala.util.Random

class EvaluationTest extends UnitSpec {
  val evalData = List(
    ("1", 1.0),
    ("+1", 1.0),
    ("-1", -1.0),
    ("14 + 12", 26),
    ("12 - 3", 9),
    ("4 * 2", 8),
    ("9 / 3", 3),
    ("(10)", 10),
    ("(-1)+1", 0),
    ("(1--1)", 2),
    ("12 == 3", false),
    ("3 == 3", true),
    ("12 != 3", true),
    ("3 != 3", false),
    ("false == false", true),
    ("true == false", false),
    ("false != false", false),
    ("true != false", true),
    ("true", true),
    ("false", false),
    ("!true", false),
    ("!false", true),
    ("var a = 100", 100),
    ("a", 100),
    ("let b = a", 100),
    ("b", 100),
    ("{a}", 100),
    ("{a + b}", 200),
    ("{var a = 3 {a = 6} a}", 6),
    ("{if(2 > 3) true else false}", false),
    ("{if(2 > 3) true else false}", false),
    ("{if(2 > 3) if(3 > 2) 7 else false else true}", true),
    ("{if(2 < 3) if(3 > 2) 7  else true}", 7),
    ("{var a = 5  var b = 0 while(a > 0) {b = b + a a = a - 1} b }", 15),
    ("{var res = 0 for i = 1 to 10 {res = res + i}res}", 55)
  )
  val variables: mutable.HashMap[VariableSymbol, Any] =
    mutable.HashMap[VariableSymbol, Any]()
  var previous: Compilation = _
  evalData.foreach { x =>
    it should s"${Random.nextInt()}" in {
      val tree = SyntaxTree.parse(x._1)
      val compilation = Compilation(tree, previous)
      previous = compilation
      assertResult(x._2)(compilation.evaluate(variables).value)
    }

  }
}
