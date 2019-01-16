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
    ("{if(2 < 3) if(3 > 2) 7  else true}", 7)
  )
  val variables: mutable.HashMap[VariableSymbol, AnyVal] = mutable.HashMap[VariableSymbol, AnyVal]()
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
