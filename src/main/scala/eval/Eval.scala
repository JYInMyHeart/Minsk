package eval

import binder._
import parser.{BindType, TokenType}
import symbol._

import scala.collection.immutable.Stack
import scala.collection.mutable
import scala.io.StdIn
import scala.util.Random

class Eval(program: BindProgram) {
  private var lastValue: Any = _

  private var locals: Stack[mutable.HashMap[VariableSymbol, Any]] = Stack()

  private var globals: mutable.HashMap[VariableSymbol, Any] = _

  private var random: Random = Random

  def this(variables: mutable.HashMap[VariableSymbol, Any],
           program: BindProgram) {
    this(program)
    globals = variables
    locals = locals.push(mutable.HashMap[VariableSymbol, Any]())
  }
  def evaluate() = eval(program.statement)

  def eval(body: BindBlockStatement): Any = {
    var labelToIndex = mutable.HashMap[BindLabel, Int]()
    for (i <- body.bindStatements.indices) {
      body.bindStatements(i) match {
        case statement: BindLabelStatement =>
          labelToIndex += statement.label -> (i)
        case _ =>
      }
    }
    var index = 0
    while (index < body.bindStatements.length) {
      val statement = body.bindStatements(index)
      (statement.getKind, statement) match {
        case (BindType.expressionStatement, s: BindExpressionStatement) =>
          evalExpressionStatement(s)
          index += 1
        case (BindType.variableDeclaration, s: BindVariableDeclaration) =>
          evalVariableDeclaration(s)
          index += 1
        case (BindType.gotoStatement, s: BindGotoStatement) =>
          index = labelToIndex(s.label)
        case (BindType.conditionGotoStatement, s: BindConditionGotoStatement) =>
          val condition = evalExpression(s.condition)
          if (condition == s.jumpIfTrue)
            index = labelToIndex(s.label)
          else
            index += 1
        case (BindType.labelStatement, _: BindLabelStatement) =>
          index += 1
        case (BindType.returnStatement, s: BindReturnStatement) =>
          lastValue =
            if (s.expression == null) null else evalExpression(s.expression)
          return lastValue
        case _ =>
          throw new Exception(s"Unexpected statement ${statement.getKind}")
      }
    }
    lastValue
  }

  def evalFuncStatement(s: BindFuncStatement): Unit = {
    lastValue = "<Function>"
  }

  def evalVariableDeclaration(statement: BindVariableDeclaration): Unit = {
    val value = evalExpression(statement.initializer)
    lastValue = value
    assign(statement.variableSymbol, value)
  }

  def evalExpressionStatement(statement: BindExpressionStatement): Unit = {
    lastValue = evalExpression(statement.bindExpression)
  }

  def evalExpression(expression: BindExpression): Any = {
    expression match {
      case node: BindLiteralExpression =>
        node.value
      case node: BindBinaryExpression =>
        val left = evalExpression(node.boundLeft)
        val right = evalExpression(node.boundRight)
        val op = node.operator.bindType
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
        if (node.variableSymbol.kind == SymbolKind.GlobalVariable)
          globals(node.variableSymbol)
        else {
          val local = locals.head
          local(node.variableSymbol)
        }
      case node: BindAssignmentExpression =>
        val value = evalExpression(node.expression)
        assign(node.variable, value)
        value
      case node: BindFuncCallExpression =>
        node.functionSymbol match {
          case BuiltinFunctions.input       => StdIn.readLine()
          case BuiltinFunctions.inputBool   => StdIn.readLine().toBoolean
          case BuiltinFunctions.inputDouble => StdIn.readLine().toDouble
          case BuiltinFunctions.inputInt    => StdIn.readLine().toInt
          case BuiltinFunctions.mPrint =>
            val msg = evalExpression(node.paramList.head)
            println(msg)
            null
          case BuiltinFunctions.rnd =>
            val max = evalExpression(node.paramList.head)
            random.nextInt(max.asInstanceOf[Int])
          case BuiltinFunctions.doubleToStr | BuiltinFunctions.boolToStr |
              BuiltinFunctions.intToStr =>
            evalExpression(node.paramList.head).toString
          case _ =>
            val local = mutable.HashMap[VariableSymbol, Any]()
            for (i <- node.paramList.indices) {
              val parameter = node.functionSymbol.parameters(i)
              val value = evalExpression(node.paramList(i))
              local += parameter -> value
            }
            locals = locals.push(local)
            val statement = program.functions(node.functionSymbol)
            val result = eval(statement)
            locals = locals.pop
            result
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

  private def assign(variable: VariableSymbol, value: Any): Unit = {
    if (variable.kind == SymbolKind.GlobalVariable)
      globals += variable -> value
    else {
      val local = locals.head
      local += variable -> value
    }
  }
}

object Eval {
  def apply(program: BindProgram): Eval = new Eval(program)
}

case class EvaluationResult(diagnosticsBag: DiagnosticsBag, value: Any)
