package eval

import binder.{BindAssignmentExpression, BindBinaryExpression, BindBlockStatement, BindExpression, BindExpressionStatement, BindForStatement, BindFuncCallExpression, BindFuncStatement, BindIfStatement, BindLiteralExpression, BindStatement, BindUnaryExpression, BindVariableExpression, BindVariableStatement, BindWhileStatement}
import parser.BindType
import symbol._

import scala.collection.mutable

class Eval(val variables: mutable.HashMap[VariableSymbol, Any]) {
  private var lastValue: Any = _

  def eval(bindStatement: BindStatement): Any = {
    evalStatement(bindStatement)
    lastValue
  }



  def evalStatement(statement: BindStatement): Unit = {
    (statement.getKind, statement) match {
      case (BindType.blockStatement, s: BindBlockStatement) =>
        evalBlockStatement(s)
      case (BindType.expressionStatement, s: BindExpressionStatement) =>
        evalExpressionStatement(s)
      case (BindType.variableDeclaration, s: BindVariableStatement) =>
        evalVariableStatement(s)
      case (BindType.ifStatement, s: BindIfStatement) =>
        evalIfStatement(s)
      case (BindType.whileStatement, s: BindWhileStatement) =>
        evalWhileStatement(s)
      case (BindType.forStatement, s: BindForStatement) =>
        evalForStatement(s)
      case (BindType.funcStatement,s:BindFuncStatement) =>
        evalFuncStatement(s)
      case _ =>
        throw new Exception(s"Unexpected statement ${statement.getKind}")
    }
  }

  def evalForStatement(statement: BindForStatement): Unit = {
    val start = evalExpression(statement.initializer)
    val end = evalExpression(statement.upper)
    for (i <- start
           .asInstanceOf[Double]
           .toInt to end.asInstanceOf[Double].toInt) {
      variables(statement.variable) = i.toDouble
      evalStatement(statement.body)
    }
  }


  def evalFuncStatement(s: BindFuncStatement): Unit = {
    lastValue = "<Function>"
  }

  def evalWhileStatement(statement: BindWhileStatement): Unit = {
    while (evalExpression(statement.condition)
             .asInstanceOf[Boolean]) evalStatement(statement.body)
  }

  def evalIfStatement(statement: BindIfStatement): Unit = {
    val value = evalExpression(statement.condition)
    value match {
      case true => evalStatement(statement.expr1)
      case false =>
        if (statement.expr2 != null)
          evalStatement(statement.expr2)
    }
  }

  def evalVariableStatement(statement: BindVariableStatement): Unit = {
    val value = evalExpression(statement.initializer)
    variables(statement.variableSymbol) = value
    lastValue = value
  }

  def evalBlockStatement(statement: BindBlockStatement): Unit = {
    for (s <- statement.bindStatements)
      evalStatement(s)
  }

  def evalExpressionStatement(statement: BindExpressionStatement): Unit = {
    lastValue = evalExpression(statement.bindExpression)
  }

  def evalExpression(expression: BindExpression): Any = {
    expression match {
      case node: BindLiteralExpression =>
        node.value match {
          case i: Double  => i
          case i: Boolean => i
          case i: Int => i.toDouble
          case _ =>
            throw new Exception(s"unknown literal type")
        }
      case node: BindBinaryExpression =>
        val left = evalExpression(node.boundLeft)
        val right = evalExpression(node.boundRight)
        val op = node.bindType.bindType
        (left, right, op) match {
          case (l: Double, r: Double, BindType.addition)       => l + r
          case (l: Double, r: Double, BindType.subtraction)    => l - r
          case (l: Double, r: Double, BindType.multiplication) => l.toDouble * r
          case (l: Double, r: Double, BindType.division)       => l.toDouble / r
          case (l: Double, r: Double, BindType.pow)            => math.pow(l, r)
          case (l: Double, r: Double, BindType.mod)            => l % r
          case (l: Double, r: Double, BindType.lt)             => l < r
          case (l: Double, r: Double, BindType.lte)            => l <= r
          case (l: Double, r: Double, BindType.gt)             => l > r
          case (l: Double, r: Double, BindType.gte)            => l >= r
          case (l: Double, r: Double, BindType.equal)          => l == r
          case (l: Double, r: Double, BindType.notequal)       => l != r
          case (l: Boolean, r: Boolean, BindType.and)          => l && r
          case (l: Boolean, r: Boolean, BindType.or)           => l || r
          case (l: Boolean, r: Boolean, BindType.equal)        => l == r
          case (l: Boolean, r: Boolean, BindType.notequal)     => l != r
          case _ =>
            throw new Exception(s"unknown literal type")
        }

      case node: BindUnaryExpression =>
        val value = evalExpression(node.boundOperand)
        (value, node.bindType.bindType) match {
          case (o: Boolean, BindType.not)     => !o
          case (o: Int, BindType.negation)    => -o
          case (o: Double, BindType.negation) => -o
          case (o: Int, BindType.identity)    => -o
          case (o: Double, BindType.identity) => o
          case _                              => throw new Exception("unknown node type")
        }
      case node: BindVariableExpression =>
        variables(node.variableSymbol)
      case node: BindAssignmentExpression =>
        val value = evalExpression(node.expression)
        variables(node.variable) = value
        value
      case node:BindFuncCallExpression =>
        val params = for(i <- node.paramList) yield evalExpression(i)
        for(i <- node.bindFuncStatement.parameters.indices){
          variables(node.bindFuncStatement.parameters(i)) = params(i)
        }
        val value = eval(node.bindFuncStatement.typeSymbol)
        value
      case _ => throw new Exception("unknown node type")
    }
  }
}

object Eval {
  def apply(variables: mutable.HashMap[VariableSymbol, Any]): Eval =
    new Eval(variables)
}

case class EvaluationResult(diagnosticsBag: DiagnosticsBag, value: Any)
