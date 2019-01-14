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
    ("(a = 10) * a", 100)
  )

  evalData.foreach{ x =>
    it should s"${Random.nextInt()}" in {
      val tree = SyntaxTree.parse(x._1)
      val variables = mutable.HashMap[VariableSymbol,AnyVal]()
      val compilation = new Compilation(tree,variables)
      assertResult(x._2)( compilation.evaluate().value)
    }

  }
}
