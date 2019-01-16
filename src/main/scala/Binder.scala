import BindType.BindType
import TokenType.TokenType
import TypeMapping._

import scala.collection.mutable

case class Binder(parent: BoundScope) {
  val diagnostics: DiagnosticsBag = DiagnosticsBag()
  var scope: BoundScope = BoundScope(parent)

  def bindStatement(statement: Statement): BindStatement = {
    (statement.getKind, statement) match {
      case (TokenType.blockStatement, s: BlockStatement) =>
        bindBlockStatement(s)
      case (TokenType.expressionStatement, s: ExpressionStatement) =>
        bindExpressionStatement(s)
      case (TokenType.`compilationUnit`, s: CompilationUnit) =>
        bindStatement(s.statement)
      case (TokenType.variableDeclaration, s: VariableDeclarationNode) =>
        bindVariableDeclaration(s)
      case (TokenType.ifStatement, s: IfStatement) =>
        bindIfStatement(s)
      case (TokenType.whileStatement, s: WhileStatement) =>
        bindWhileStatement(s)
      case (TokenType.forStatement, s: ForStatement) =>
        bindForStatement(s)
      case _ =>
        throw new LexerException(s"unexpected syntax ${statement.getKind}")
    }
  }

  def bindExpression(expression: Expression, targetType: String): BindExpression = {
    val res = bindExpression(expression)
    if (res.bindTypeClass != targetType)
      diagnostics.reportCannotConvert(null, res.bindTypeClass, targetType)
    res
  }

  def bindExpression(tree: Expression): BindExpression = {
    (tree.getKind, tree) match {
      case (TokenType.nameExpression, n: NameNode) =>
        bindNameExpression(n)
      case (TokenType.assignmentExpression, n: AssignmentNode) =>
        bindAssignmentExpression(n)
      case (TokenType.binaryExpression, n: BinaryNode) =>
        bindBinaryExpression(n)
      case (TokenType.unaryExpression, n: UnaryNode) =>
        bindUnaryExpression(n)
      case (TokenType.numberExpression, n: LiteralNode) =>
        bindLiteralExpression(n)
      case (TokenType.braceExpression, n: BraceNode) =>
        bindExpression(n.op)
      case _ =>
        throw new LexerException(s"unexpected syntax ${tree.getKind}")
    }
  }

  private def bindForStatement(statement: ForStatement): BindForStatement = {
    val low = bindExpression(statement.low)
    val variableSymbol = VariableSymbol(statement.identifier.value, low.bindTypeClass, isReadOnly = false)
    scope.tryDeclare(variableSymbol)
    val upper = bindExpression(statement.upper)
    val body = bindStatement(statement.body)
    BindForStatement(variableSymbol, low, upper, body)
  }

  private def bindWhileStatement(statement: WhileStatement): BindWhileStatement = {
    val condition = bindExpression(statement.condition, bool)
    val body = bindStatement(statement.body)
    BindWhileStatement(condition, body)
  }

  private def bindIfStatement(statement: IfStatement): BindIfStatement = {
    val condition = bindExpression(statement.condition, bool)
    val thenStatement = bindStatement(statement.expr1)
    val elseStatement = if (statement.expr2 == null) null else bindStatement(statement.expr2)
    BindIfStatement(condition, thenStatement, elseStatement)

  }

  private def bindVariableDeclaration(statement: VariableDeclarationNode): BindVariableStatement = {
    val name = statement.identifier.value
    val isReadOnly = statement.keyword.tokenType == TokenType.letKeyword
    val initializer = bindExpression(statement.expression)
    val variable = VariableSymbol(name, initializer.bindTypeClass, isReadOnly)
    if (!scope.tryDeclare(variable))
      diagnostics.reportVariableAlreadyDeclared(statement.identifier.span, name)
    BindVariableStatement(variable, initializer)
  }

  private def bindBlockStatement(statement: BlockStatement): BindBlockStatement = {
    var statements: List[BindStatement] = List()
    scope = BoundScope(scope)
    for (s <- statement.statements) {
      val statement = bindStatement(s)
      statements :+= statement
    }
    scope = scope.parent
    BindBlockStatement(statements)
  }

  private def bindExpressionStatement(statement: ExpressionStatement): BindExpressionStatement = {
    val expression = bindExpression(statement.expression)
    BindExpressionStatement(expression)
  }

  private def bindNameExpression(node: NameNode): BindExpression = {
    val name = node.identifierToken.value
    if (name == null || name.isEmpty)
      return BindLiteralExpression(0)
    val variable = scope.tryLookup(name)
    if (variable == null) {
      diagnostics.reportUndefinedName(node.identifierToken.span, name)
      return BindLiteralExpression(0)
    }
    BindVariableExpression(variable)
  }

  private def bindAssignmentExpression(node: AssignmentNode): BindExpression = {
    val name = node.identifierToken.value
    val boundExpression = bindExpression(node.expression)
    val existingVariable = scope.tryLookup(name)
    if (existingVariable == null) {
      diagnostics.reportUndefinedName(node.identifierToken.span, name)
      return boundExpression
    }
    if (existingVariable.isReadOnly)
      diagnostics.reportCannotAssign(node.equalsToken.span, name)

    if (boundExpression.bindTypeClass != existingVariable.varType) {
      diagnostics.reportCannotConvert(node.equalsToken.span, boundExpression.bindTypeClass, existingVariable.varType)
      return boundExpression
    }
    BindAssignmentExpression(existingVariable, boundExpression)
  }

  private def bindLiteralExpression(node: LiteralNode): BindExpression = {
    val value = node.value.value match {
      case "true" => true
      case "false" => false
      case x => x.toDouble
    }
    BindLiteralExpression(value)
  }

  private def bindBinaryExpression(node: BinaryNode): BindExpression = {
    val boundLeft = bindExpression(node.left)
    val boundRight = bindExpression(node.right)
    val boundOperator =
      BoundBinaryOperator.bind(
        node.op.tokenType,
        boundLeft.bindTypeClass,
        boundRight.bindTypeClass
      )
    if (boundOperator == null) {
      diagnostics.reportUndefinedBinaryOperator(
        node.op.span,
        node.op.value,
        boundLeft.bindTypeClass,
        boundRight.bindTypeClass
      )
      return boundLeft
    }
    BindBinaryExpression(boundOperator, boundLeft, boundRight)
  }

  private def bindUnaryExpression(node: UnaryNode): BindExpression = {
    val boundOperand = bindExpression(node.operand)
    val boundOperatorKind =
      BoundUnaryOperator.bind(
        node.op.getKind,
        boundOperand.bindTypeClass
      )
    if (boundOperatorKind == null) {
      diagnostics.reportUndefinedUnaryOperator(
        node.op.asInstanceOf[Tokens].span,
        node.op.asInstanceOf[Tokens].value,
        boundOperand.bindTypeClass

      )
      return boundOperand
    }
    BindUnaryExpression(boundOperatorKind, boundOperand)
  }
}

object Binder {
  def bindGlobalScope(previous: BoundGlobalScope,
                      syntax: CompilationUnit): BoundGlobalScope = {
    val parentScope = createParentScope(previous)
    val binder = Binder(parentScope)
    val expression = binder.bindStatement(syntax.statement)
    val variables = binder.scope.getDeclaredVariables
    val diagnostics = binder.diagnostics
    if (previous != null)
      diagnostics concat previous.diagnostics
    BoundGlobalScope(previous, diagnostics, variables, expression)
  }

  def createParentScope(previous: BoundGlobalScope): BoundScope = {
    var pre = previous
    val stack = mutable.Stack[BoundGlobalScope]()
    while (pre != null) {
      stack.push(pre)
      pre = pre.previous
    }
    var parent: BoundScope = null
    while (stack.nonEmpty) {
      pre = stack.pop()
      val scope = BoundScope(parent)
      for (v <- pre.variables)
        scope.tryDeclare(v)
      parent = scope
    }
    parent
  }
}


abstract class BoundNode {
  def bindTypeClass: String
}

abstract class BindExpression extends BoundNode


abstract class BindStatement extends BoundNode {
  def getKind: BindType.BindType
}

case class BindExpressionStatement(bindExpression: BindExpression) extends BindStatement {
  override def bindTypeClass: String = bindExpression.bindTypeClass

  override def getKind: BindType.BindType = BindType.expressionStatement
}

case class BindBlockStatement(bindStatements: List[BindStatement]) extends BindStatement {
  override def bindTypeClass: String = null

  override def getKind: BindType.BindType = BindType.blockStatement
}

case class BindVariableStatement(variableSymbol: VariableSymbol,
                                 initializer: BindExpression) extends BindStatement {
  override def bindTypeClass: String = variableSymbol.varType

  override def getKind: BindType = BindType.variableDeclaration
}

case class BindIfStatement(condition: BindExpression,
                           expr1: BindStatement,
                           expr2: BindStatement) extends BindStatement {
  override def getKind: BindType = BindType.ifStatement

  override def bindTypeClass: String = expr1.bindTypeClass
}

case class BindWhileStatement(condition: BindExpression,
                              body: BindStatement) extends BindStatement {
  override def getKind: BindType = BindType.whileStatement

  override def bindTypeClass: String = body.bindTypeClass
}

case class BindForStatement(variable: VariableSymbol,
                            initializer: BindExpression,
                            upper: BindExpression,
                            body: BindStatement) extends BindStatement {
  override def getKind: BindType = BindType.forStatement

  override def bindTypeClass: String = body.bindTypeClass
}


case class BindBinaryExpression(bindType: BoundBinaryOperator,
                                boundLeft: BindExpression,
                                boundRight: BindExpression) extends BindExpression {
  override def bindTypeClass: String = bindType.result
}

case class BindUnaryExpression(bindType: BoundUnaryOperator,
                               boundOperand: BindExpression) extends BindExpression {
  override def bindTypeClass: String = bindType.result
}

case class BindLiteralExpression(value: AnyVal) extends BindExpression {
  override def bindTypeClass: String = value.getClass.getSimpleName
}

sealed class BoundBinaryOperator(val tokenType: TokenType,
                                 val bindType: BindType,
                                 val left: String,
                                 val right: String,
                                 val result: String)


case class BindVariableExpression(variableSymbol: VariableSymbol) extends BindExpression {
  override def bindTypeClass: String = variableSymbol.varType
}

case class BindAssignmentExpression(variable: VariableSymbol,
                                    expression: BindExpression) extends BindExpression {
  override def bindTypeClass: String = expression.bindTypeClass
}

object BoundBinaryOperator {


  def apply(
             tokenType: TokenType,
             bindType: BindType,
             left: String,
             right: String,
             result: String
           ): BoundBinaryOperator =
    new BoundBinaryOperator(
      tokenType,
      bindType,
      left,
      right,
      result
    )

  private[this] def binaryOperators: List[BoundBinaryOperator] =
    List(
      BoundBinaryOperator(TokenType.add, BindType.addition, double, double, double),
      BoundBinaryOperator(TokenType.sub, BindType.subtraction, double, double, double),
      BoundBinaryOperator(TokenType.div, BindType.division, double, double, double),
      BoundBinaryOperator(TokenType.plus, BindType.multiplication, double, double, double),
      BoundBinaryOperator(TokenType.pow, BindType.pow, double, double, double),
      BoundBinaryOperator(TokenType.mod, BindType.mod, double, double, double),
      BoundBinaryOperator(TokenType.lt, BindType.lt, double, double, bool),
      BoundBinaryOperator(TokenType.gt, BindType.gt, double, double, bool),
      BoundBinaryOperator(TokenType.lte, BindType.lte, double, double, bool),
      BoundBinaryOperator(TokenType.gte, BindType.gte, double, double, bool),
      BoundBinaryOperator(TokenType.equal, BindType.equal, double, double, bool),
      BoundBinaryOperator(TokenType.equal, BindType.equal, bool, bool, bool),
      BoundBinaryOperator(TokenType.notequal, BindType.notequal, double, double, bool),
      BoundBinaryOperator(TokenType.notequal, BindType.notequal, bool, bool, bool),
      BoundBinaryOperator(TokenType.and, BindType.and, bool, bool, bool),
      BoundBinaryOperator(TokenType.or, BindType.or, bool, bool, bool)
    )

  def bind(tokenType: TokenType, left: String, right: String): BoundBinaryOperator = {
    val binaryOperator = binaryOperators.filter(x => x.tokenType == tokenType && x.left == left && x.right == right)
    if (binaryOperator.nonEmpty)
      binaryOperator.last
    else
      null
  }
}

sealed class BoundUnaryOperator(val tokenType: TokenType,
                                val bindType: BindType,
                                val operand: String,
                                val result: String)

object BoundUnaryOperator {


  def apply(
             tokenType: TokenType,
             bindType: BindType,
             operand: String,
             result: String
           ): BoundUnaryOperator = new BoundUnaryOperator(
    tokenType,
    bindType,
    operand,
    result
  )

  private[this] def unaryOperators: List[BoundUnaryOperator] =
    List(
      BoundUnaryOperator(TokenType.not, BindType.not, bool, bool),
      BoundUnaryOperator(TokenType.sub, BindType.negation, double, double),
      BoundUnaryOperator(TokenType.sub, BindType.negation, int, int),
      BoundUnaryOperator(TokenType.add, BindType.identity, double, double),
      BoundUnaryOperator(TokenType.add, BindType.identity, int, int)
    )

  def bind(tokenType: TokenType, operand: String): BoundUnaryOperator = {
    val unaryOperator = unaryOperators.filter(x => x.tokenType == tokenType && x.operand == operand)
    if (unaryOperator.nonEmpty)
      unaryOperator.last
    else
      null
  }
}

object TypeMapping {
  val bool = "Boolean"
  val int = "Integer"
  val double = "Double"
}


