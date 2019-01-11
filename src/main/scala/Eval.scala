import scala.collection.mutable

class Eval(val variables: mutable.HashMap[VariableSymbol, AnyVal]) {
  def eval(expression: BindExpression): AnyVal = {
    expression match {
      case node: BindLiteralExpression =>
        node.value match {
          case i: Int => i
          case i: Boolean => i
          case _ =>
            throw new Exception(s"unknown literal type")
        }
      case node: BindBinaryExpression =>
        val left = eval(node.boundLeft)
        val right = eval(node.boundRight)
        val op = node.bindType.bindType
        (left, right, op) match {
          case (l: Int, r: Int, BindType.addition) => l + r
          case (l: Int, r: Int, BindType.subtraction) => l - r
          case (l: Int, r: Int, BindType.multiplication) => l.toDouble * r
          case (l: Int, r: Int, BindType.division) => l.toDouble / r
          case (l: Int, r: Int, BindType.pow) => math.pow(l, r)
          case (l: Int, r: Int, BindType.mod) => l % r
          case (l: Int, r: Int, BindType.lt) => l < r
          case (l: Int, r: Int, BindType.lte) => l <= r
          case (l: Int, r: Int, BindType.gt) => l > r
          case (l: Int, r: Int, BindType.gte) => l >= r
          case (l: Int, r: Int, BindType.equal) => l == r
          case (l: Int, r: Int, BindType.notequal) => l != r
          case (l: Boolean, r: Boolean, BindType.and) => l && r
          case (l: Boolean, r: Boolean, BindType.or) => l || r
          case (l: Boolean, r: Boolean, BindType.equal) => l == r
          case (l: Boolean, r: Boolean, BindType.notequal) => l != r
          case _ =>
            throw new Exception(s"unknown literal type")
        }

      case node: BindUnaryExpression =>
        val value = eval(node.boundOperand)
        (value, node.bindType.bindType) match {
          case (o: Boolean, BindType.not) => !o
          case (o: Int, BindType.negation) => -o
          case (o: Int, BindType.identity) => o
          case _ => throw new LexerException("unknown node type")
        }
      case node: BindVariableExpression =>
        variables(node.variableSymbol)
      case node: BindAssignmentExpression =>
        val value = eval(node.expression)
        variables(node.variable) = value
        value
      case _ => throw new LexerException("unknown node type")
    }
  }
}

object Eval {
  def apply(variables: mutable.HashMap[VariableSymbol, AnyVal]): Eval = new Eval(variables)
}

case class EvaluationResult(diagnosticsBag: DiagnosticsBag,
                            value: AnyVal)


