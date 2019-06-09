package eval

import binder._
import parser.BindType
import symbol._

import scala.collection.mutable
import scala.io.StdIn
import scala.util.Random

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
      case _ =>
        throw new Exception(s"Unexpected statement ${statement.getKind}")
    }
  }

  def evalForStatement(statement: BindForStatement): Unit = {
    val start = evalExpression(statement.initializer)
    val end = evalExpression(statement.upper)
    for (i <- start
           .asInstanceOf[Int]
            to end.asInstanceOf[Int]) {
      variables(statement.variable) = i
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
          case i: Int     => i
          case i: String  => i
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
          case (l: Double, r: Double, BindType.multiplication) => l * r
          case (l: Double, r: Double, BindType.division)       => l / r
          case (l: Double, r: Double, BindType.pow)            => math.pow(l, r)
          case (l: Double, r: Double, BindType.mod)            => l % r
          case (l: Double, r: Double, BindType.lt)             => l < r
          case (l: Double, r: Double, BindType.lte)            => l <= r
          case (l: Double, r: Double, BindType.gt)             => l > r
          case (l: Double, r: Double, BindType.gte)            => l >= r
          case (l: Double, r: Double, BindType.equal)          => l == r
          case (l: Double, r: Double, BindType.notequal)       => l != r

          case (l: Int, r: Double, BindType.addition)       => l + r
          case (l: Int, r: Double, BindType.subtraction)    => l - r
          case (l: Int, r: Double, BindType.multiplication) => l * r
          case (l: Int, r: Double, BindType.division)       => l / r
          case (l: Int, r: Double, BindType.pow)            => math.pow(l, r)
          case (l: Int, r: Double, BindType.mod)            => l % r
          case (l: Int, r: Double, BindType.lt)             => l < r
          case (l: Int, r: Double, BindType.lte)            => l <= r
          case (l: Int, r: Double, BindType.gt)             => l > r
          case (l: Int, r: Double, BindType.gte)            => l >= r
          case (l: Int, r: Double, BindType.equal)          => l == r
          case (l: Int, r: Double, BindType.notequal)       => l != r

          case (l: Double, r: Int, BindType.addition)       => l + r
          case (l: Double, r: Int, BindType.subtraction)    => l - r
          case (l: Double, r: Int, BindType.multiplication) => l * r
          case (l: Double, r: Int, BindType.division)       => l / r
          case (l: Double, r: Int, BindType.pow)            => math.pow(l, r)
          case (l: Double, r: Int, BindType.mod)            => l % r
          case (l: Double, r: Int, BindType.lt)             => l < r
          case (l: Double, r: Int, BindType.lte)            => l <= r
          case (l: Double, r: Int, BindType.gt)             => l > r
          case (l: Double, r: Int, BindType.gte)            => l >= r
          case (l: Double, r: Int, BindType.equal)          => l == r
          case (l: Double, r: Int, BindType.notequal)       => l != r

          case (l: Int, r: Int, BindType.addition)       => l + r
          case (l: Int, r: Int, BindType.subtraction)    => l - r
          case (l: Int, r: Int, BindType.multiplication) => l * r
          case (l: Int, r: Int, BindType.division)       => l / r
          case (l: Int, r: Int, BindType.pow)            => math.pow(l, r).toInt
          case (l: Int, r: Int, BindType.mod)            => l % r
          case (l: Int, r: Int, BindType.lt)             => l < r
          case (l: Int, r: Int, BindType.lte)            => l <= r
          case (l: Int, r: Int, BindType.gt)             => l > r
          case (l: Int, r: Int, BindType.gte)            => l >= r
          case (l: Int, r: Int, BindType.equal)          => l == r
          case (l: Int, r: Int, BindType.notequal)       => l != r

          case (l: Boolean, r: Boolean, BindType.and)      => l && r
          case (l: Boolean, r: Boolean, BindType.or)       => l || r
          case (l: Boolean, r: Boolean, BindType.equal)    => l == r
          case (l: Boolean, r: Boolean, BindType.notequal) => l != r
          case (l: String, r: String, BindType.notequal)   => l != r
          case (l: String, r: String, BindType.equal)      => l == r
          case (l: String, r: String, BindType.addition)   => l + r
          case _ =>
            throw new Exception(s"unknown literal type")
        }

      case node: BindUnaryExpression =>
        val value = evalExpression(node.boundOperand)
        (value, node.bindType.bindType) match {
          case (o: Boolean, BindType.not)     => !o
          case (o: Int, BindType.negation)    => -o
          case (o: Double, BindType.negation) => -o
          case (o: Int, BindType.identity)    => o
          case (o: Double, BindType.identity) => o
          case _                              => throw new Exception("unknown node type")
        }
      case node: BindVariableExpression =>
        variables(node.variableSymbol)
      case node: BindAssignmentExpression =>
        val value = evalExpression(node.expression)
        variables(node.variable) = value
        value
      case node: BindFuncCallExpression =>
        node.functionSymbol match {
          case BuiltinFunctions.input => StdIn.readLine()
          case BuiltinFunctions.mPrint =>
            val msg = evalExpression(node.paramList.head)
            println(msg)
            null
          case BuiltinFunctions.rnd =>
            val max = evalExpression(node.paramList.head)
            Random.nextInt(max.asInstanceOf[Double].toInt)
          case _ =>
            throw new Exception(s"Unexpected function ${node.functionSymbol}")
        }
      case node: BindConversionExpression =>
        val value = evalExpression(node.bindExpression)
        node.typeSymbol match {
          case TypeSymbol.Bool   => value.asInstanceOf[Boolean]
          case TypeSymbol.Int    => value.asInstanceOf[Int]
          case TypeSymbol.Double => value.asInstanceOf[Double]
          case TypeSymbol.String => value.asInstanceOf[String]
          case _ =>
            throw new Exception(s"Unexpected type ${node.typeSymbol}")
        }

      case _ => throw new Exception("unknown node type")
    }
  }
}

object Eval {
  def apply(variables: mutable.HashMap[VariableSymbol, Any]): Eval =
    new Eval(variables)
}

case class EvaluationResult(diagnosticsBag: DiagnosticsBag, value: Any)
