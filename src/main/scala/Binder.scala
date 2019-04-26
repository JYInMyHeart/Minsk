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
      case (TokenType.funcStatement, s: FuncStatement) =>
        bindFuncStatement(s)
      case _ =>
        throw new LexerException(s"unexpected syntax ${statement.getKind}")
    }
  }

  def bindExpression(expression: Expression,
                     targetType: String): BindExpression = {
    val res = bindExpression(expression)
    if (res.bindTypeClass != targetType)
      diagnostics.reportCannotConvert(null, res.bindTypeClass, targetType)
    res
  }

  def bindFuncCallExpression(n: FunctionCallNode): BindFuncCallExpression = {
    val name = n.identifier.value
    val function = scope.tryLookupFunc(name)

    if (function.param.length != n.expressions.length) {
      diagnostics.reportParamMismatch(n.identifier.span,
                                      function.param,
                                      n.expressions)
    }
    BindFuncCallExpression(function,
                           for (i <- n.expressions) yield bindExpression(i))
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
      case (TokenType.funcCallExpression, n: FunctionCallNode) =>
        bindFuncCallExpression(n)
      case _ =>
        throw new LexerException(s"unexpected syntax ${tree.getKind}")
    }
  }

  private def bindFuncStatement(statement: FuncStatement): BindFuncStatement = {
    val funcName =
      VariableSymbol(statement.identifier.value,
                     statement.returnType.paramType.value,
                     isReadOnly = true)
    val paramsList = for (param <- statement.parameters) yield {
      VariableSymbol(param.id.value, param.paramType.value, isReadOnly = false)
    }

    scope = BoundScope(this.scope)
    paramsList.foreach(scope.tryDeclare(_))
    val body = bindStatement(statement.body)
    scope = scope.parent
    if (body.bindTypeClass != statement.returnType.paramType.value)
      diagnostics.reportFunctionTypeMismatched(
        statement.identifier.span,
        statement.identifier.value,
        statement.returnType.paramType.value,
        body.bindTypeClass)
    val function = BindFuncStatement(funcName, paramsList, body)
    if (!scope.tryDeclare(function))
      diagnostics.reportVariableAlreadyDeclared(
        statement.identifier.span,
        statement.returnType.paramType.value
      )

    function
  }

  private def bindForStatement(statement: ForStatement): BindForStatement = {
    val low = bindExpression(statement.low)
    val variableSymbol = VariableSymbol(statement.identifier.value,
                                        low.bindTypeClass,
                                        isReadOnly = false)
    scope.tryDeclare(variableSymbol)
    val upper = bindExpression(statement.upper)
    val body = bindStatement(statement.body)
    BindForStatement(variableSymbol, low, upper, body)
  }

  private def bindWhileStatement(
      statement: WhileStatement): BindWhileStatement = {
    val condition = bindExpression(statement.condition, bool)
    val body = bindStatement(statement.body)
    BindWhileStatement(condition, body)
  }

  private def bindIfStatement(statement: IfStatement): BindIfStatement = {
    val condition = bindExpression(statement.condition, bool)
    val thenStatement = bindStatement(statement.expr1)
    val elseStatement =
      if (statement.expr2 == null) null else bindStatement(statement.expr2)
    BindIfStatement(condition, thenStatement, elseStatement)

  }

  private def bindVariableDeclaration(
      statement: VariableDeclarationNode): BindVariableStatement = {
    val name = statement.identifier.value
    val isReadOnly = statement.keyword.tokenType == TokenType.letKeyword
    val initializer = bindExpression(statement.expression)
    val variable = VariableSymbol(name, initializer.bindTypeClass, isReadOnly)
    if (!scope.tryDeclare(variable))
      diagnostics.reportVariableAlreadyDeclared(statement.identifier.span, name)
    BindVariableStatement(variable, initializer)
  }

  private def bindBlockStatement(
      statement: BlockStatement): BindBlockStatement = {
    var statements: List[BindStatement] = List()
    scope = BoundScope(this.scope)
    for (s <- statement.statements) {
      val statement = bindStatement(s)
      statements :+= statement
    }
    scope = scope.parent
    BindBlockStatement(statements)
  }

  private def bindExpressionStatement(
      statement: ExpressionStatement): BindExpressionStatement = {
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
      diagnostics.reportCannotConvert(node.equalsToken.span,
                                      boundExpression.bindTypeClass,
                                      existingVariable.varType)
      return boundExpression
    }
    BindAssignmentExpression(existingVariable, boundExpression)
  }

  private def bindLiteralExpression(node: LiteralNode): BindExpression = {
    val value = node.value.value match {
      case "true"  => true
      case "false" => false
      case x       => x.toDouble
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
    val functions = binder.scope.getDeclaredFunctions
    val diagnostics = binder.diagnostics
    if (previous != null)
      diagnostics concat previous.diagnostics
    BoundGlobalScope(previous, diagnostics, variables, functions, expression)
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

case class BindExpressionStatement(bindExpression: BindExpression)
    extends BindStatement {
  override def bindTypeClass: String = bindExpression.bindTypeClass

  override def getKind: BindType.BindType = BindType.expressionStatement
}

case class BindBlockStatement(bindStatements: List[BindStatement])
    extends BindStatement {
  override def bindTypeClass: String = bindStatements.last.bindTypeClass

  override def getKind: BindType.BindType = BindType.blockStatement
}

case class BindVariableStatement(variableSymbol: VariableSymbol,
                                 initializer: BindExpression)
    extends BindStatement {
  override def bindTypeClass: String = variableSymbol.varType

  override def getKind: BindType = BindType.variableDeclaration
}

case class BindIfStatement(condition: BindExpression,
                           expr1: BindStatement,
                           expr2: BindStatement)
    extends BindStatement {
  override def getKind: BindType = BindType.ifStatement

  override def bindTypeClass: String = expr1.bindTypeClass
}

case class BindWhileStatement(condition: BindExpression, body: BindStatement)
    extends BindStatement {
  override def getKind: BindType = BindType.whileStatement

  override def bindTypeClass: String = body.bindTypeClass
}

case class BindForStatement(variable: VariableSymbol,
                            initializer: BindExpression,
                            upper: BindExpression,
                            body: BindStatement)
    extends BindStatement {
  override def getKind: BindType = BindType.forStatement

  override def bindTypeClass: String = body.bindTypeClass
}

case class BindFuncStatement(identifier: VariableSymbol,
                             param: List[VariableSymbol],
                             body: BindStatement)
    extends BindStatement {
  override def getKind: BindType = BindType.funcStatement

  override def bindTypeClass: String = body.bindTypeClass
}

case class BindBinaryExpression(bindType: BoundBinaryOperator,
                                boundLeft: BindExpression,
                                boundRight: BindExpression)
    extends BindExpression {
  override def bindTypeClass: String = bindType.result
}

case class BindUnaryExpression(bindType: BoundUnaryOperator,
                               boundOperand: BindExpression)
    extends BindExpression {
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

case class BindVariableExpression(variableSymbol: VariableSymbol)
    extends BindExpression {
  override def bindTypeClass: String = variableSymbol.varType
}

case class BindAssignmentExpression(variable: VariableSymbol,
                                    expression: BindExpression)
    extends BindExpression {
  override def bindTypeClass: String = expression.bindTypeClass
}

case class BindFuncCallExpression(bindFuncStatement: BindFuncStatement,
                                  paramList: List[BindExpression])
    extends BindExpression {
  override def bindTypeClass: String = bindFuncStatement.bindTypeClass
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

  def bind(tokenType: TokenType,
           left: String,
           right: String): BoundBinaryOperator = {
    val bindType = getBindType(tokenType)

    if (left == right) {
      if (comparableOperations.contains(bindType))
        BoundBinaryOperator(tokenType,
                            getBindType(tokenType),
                            left,
                            right,
                            bool)
      else if (computeOperations.contains(bindType))
        BoundBinaryOperator(tokenType,
                            getBindType(tokenType),
                            left,
                            right,
                            left)
      else if (logicOperations.contains(bindType))
        BoundBinaryOperator(tokenType, getBindType(tokenType), bool, bool, bool)
      else
        null
    } else {
      if (convert(left, right))
        BoundBinaryOperator(tokenType,
                            getBindType(tokenType),
                            right,
                            right,
                            right)
      else if (convert(right, left))
        BoundBinaryOperator(tokenType, getBindType(tokenType), left, left, left)
      else
        null
    }
  }

  def convert(left: String, right: String): Boolean = (left, right) match {
    case (`int`, `double`) => true
    case _                 => false
  }

  def getBindType(tokenType: TokenType): BindType.Value = {
    tokenType match {
      case TokenType.add      => BindType.addition
      case TokenType.sub      => BindType.subtraction
      case TokenType.mul      => BindType.multiplication
      case TokenType.div      => BindType.division
      case TokenType.pow      => BindType.pow
      case TokenType.mod      => BindType.mod
      case TokenType.lt       => BindType.lt
      case TokenType.gt       => BindType.gt
      case TokenType.lte      => BindType.lte
      case TokenType.gte      => BindType.gte
      case TokenType.equal    => BindType.equal
      case TokenType.notequal => BindType.notequal
      case TokenType.and      => BindType.and
      case TokenType.or       => BindType.or
    }
  }

  private val logicOperations = List(BindType.and, BindType.or)

  private val comparableOperations = List(BindType.lt,
                                          BindType.gt,
                                          BindType.lte,
                                          BindType.gte,
                                          BindType.equal,
                                          BindType.notequal)

  private val computeOperations = List(BindType.addition,
                                       BindType.subtraction,
                                       BindType.multiplication,
                                       BindType.division,
                                       BindType.pow,
                                       BindType.mod)
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
    val unaryOperator = unaryOperators.filter(x =>
      x.tokenType == tokenType && x.operand == operand)
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
