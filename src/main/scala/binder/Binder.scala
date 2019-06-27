package binder

import eval.DiagnosticsBag
import parser.BindType.BindType
import parser.TokenType.TokenType
import parser._
import symbol.{BuiltinFunctions, FunctionSymbol, ParameterSymbol, TypeSymbol, VariableSymbol}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Binder(parent: BindScope,
                  function:FunctionSymbol) {

  def lookupTyoe(name: String): TypeSymbol = name match {
    case "bool" =>
      TypeSymbol.Bool
    case "int" =>
      TypeSymbol.Int
    case "String" =>
      TypeSymbol.String
    case _ => null
  }

  def bindTypeClause(node: TypeClauseNode):TypeSymbol = {
    if(node == null) return null
    val functionType = lookupTyoe(node.identifier.text)
    if(functionType == null)
      diagnostics.reportUndefinedType(node.identifier.text,node.identifier.getSpan)
    functionType
  }

  def bindFunctionDeclaration(node: FunctionDeclarationNode): Unit = {
    val parameters = ListBuffer[ParameterSymbol]()
    val seenParameterNames = mutable.HashSet[String]()
    for(parameterNode <- node.parameters){
      val parameterName = parameterNode.identifier.text
      val parameterType = bindTypeClause(parameterNode.parameterType)
      if(!seenParameterNames.add(parameterName)){
        diagnostics.reportParameterAlreadyDeclared(parameterNode.getSpan,parameterName)
      }else{
        val parameter = ParameterSymbol(parameterName,parameterType)
        parameters += parameter
      }
    }
    val bindType = bindTypeClause(node.functionType)
    val functionType = if(bindType == null) TypeSymbol.Void else bindType
    val declaredFuntion = new FunctionSymbol(node.identifier.text,parameters.toList,functionType)
    if(!scope.tryDeclare(declaredFuntion))
      diagnostics.reportFunctionAlreadyDeclared(node.identifier.span,declaredFuntion.name)
  }

  val diagnostics: DiagnosticsBag = DiagnosticsBag()
  var scope: BindScope = BindScope(parent)
  var _function:FunctionSymbol = initFunction

  def initFunction:FunctionSymbol = {
    if(function != null)
      for(p <- function.parameters)
        scope.tryDeclare(p)
    function
  }

  def bindStatement(statement: Statement): BindStatement = {
    (statement.getKind, statement) match {
      case (TokenType.blockStatement, s: BlockStatement) =>
        bindBlockStatement(s)
      case (TokenType.expressionStatement, s: ExpressionStatement) =>
        bindExpressionStatement(s)
      case (TokenType.`compilationUnit`, s: CompilationUnit) =>
        bindStatement(s)
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
        throw new Exception(s"unexpected syntax ${statement.getKind}")
    }
  }

  def bindExpression(expression: Expression,
                     targetType: TypeSymbol): BindExpression = {
    val res = bindExpression(expression)
    if (res.getType != targetType)
      diagnostics.reportCannotConvert(null, res.getType, targetType)
    res
  }

  def bindConversion(typeSymbol: TypeSymbol,
                     expression: Expression): BindExpression = {
    val expr = bindExpression(expression)
    val conversion = Conversion.classify(expr.getType, typeSymbol)
    if (!conversion.exists) {
      diagnostics.reportCannotConvert(expression.getSpan,
                                      expr.getType,
                                      typeSymbol)
      return BindErrorExpression()
    }
    BindConversionExpression(typeSymbol, expr)
  }

  def bindFuncCallExpression(n: FunctionCallNode): BindExpression = {
    val typeSymbol = Binder.lookupType(n.identifier.text)
    if (n.arguments.length == 1 && typeSymbol != null) {
      bindConversion(typeSymbol, n.arguments.head)
      return BindErrorExpression()
    }
    val bindArguments = n.arguments.map(bindExpression)
    val functionSymbol = scope.tryLookupFunction(n.identifier.text)
    if (functionSymbol == null) {
      diagnostics.reportUndefinedFunction(n.identifier.getSpan,
                                          n.identifier.text)
      return BindErrorExpression()
    }
    if (n.arguments.length != functionSymbol.parameters.length) {
      diagnostics.reportFunctionParametersLengthMismatched(
        n.getSpan,
        functionSymbol.name,
        functionSymbol.parameters.length,
        n.arguments.length)
      return BindErrorExpression()
    }
    for (i <- bindArguments.indices) {
      val argument = bindArguments(i)
      val parameter = functionSymbol.parameters(i)

      if (argument.getType != parameter.typeSymbol) {
        diagnostics.reportFunctionTypeMismatched(n.getSpan,
                                                 functionSymbol.name,
                                                 argument.getType,
                                                 parameter.typeSymbol)
        return BindErrorExpression()
      }
    }
    BindFuncCallExpression(functionSymbol, bindArguments)
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
        throw new Exception(s"unexpected syntax ${tree.getKind}")
    }
  }

  private def bindFuncStatement(statement: FuncStatement): BindFuncStatement = {
//    val funcName =
//      new VariableSymbol(statement.identifier.text,
//                     statement.returnType.paramType.text,
//                     isReadOnly = true)
//    val paramsList = for (param <- statement.parameters) yield {
//      new VariableSymbol(param.id.text, param.paramType.text, isReadOnly = false)
//    }
//
//    scope = BoundScope(this.scope)
//    paramsList.foreach(scope.tryDeclare(_))
//    val body = bindStatement(statement.body)
//    scope = scope.parent
//    if (body.getType != statement.returnType.paramType.value)
//      diagnostics.reportFunctionTypeMismatched(
//        statement.identifier.span,
//        statement.identifier.text,
//        statement.returnType.paramType.text,
//        body.getType)
//    val function = BindFuncStatement(funcName, paramsList, body)
//    if (!scope.tryDeclare(function))
//      diagnostics.reportVariableAlreadyDeclared(
//        statement.identifier.span,
//        statement.returnType.paramType.text
//      )
//
//    function
    null
  }

  private def bindForStatement(statement: ForStatement): BindForStatement = {
    val low = bindExpression(statement.low)
    val variableSymbol =
      bindVariable(statement.identifier, TypeSymbol.Int, isReadOnly = false)
    scope.tryDeclare(variableSymbol)
    val upper = bindExpression(statement.upper)
    val body = bindStatement(statement.body)
    BindForStatement(variableSymbol, low, upper, body)
  }

  private def bindWhileStatement(
      statement: WhileStatement): BindWhileStatement = {
    val condition = bindExpression(statement.condition, TypeSymbol.Bool)
    val body = bindStatement(statement.body)
    BindWhileStatement(condition, body)
  }

  private def bindIfStatement(statement: IfStatement): BindIfStatement = {
    val condition = bindExpression(statement.condition, TypeSymbol.Bool)
    val thenStatement = bindStatement(statement.expr1)
    val elseStatement =
      if (statement.expr2 == null) null else bindStatement(statement.expr2)
    BindIfStatement(condition, thenStatement, elseStatement)

  }

  private def bindVariableDeclaration(
      statement: VariableDeclarationNode): BindVariableStatement = {
    val name = statement.identifier.text
    val isReadOnly = statement.keyword.tokenType == TokenType.letKeyword
    val initializer = bindExpression(statement.expression)
    val variable = new VariableSymbol(name, initializer.getType, isReadOnly)
    if (!scope.tryDeclare(variable))
      diagnostics.reportVariableAlreadyDeclared(statement.identifier.span, name)
    BindVariableStatement(variable, initializer)
  }

  private def bindVariable(identifier: Token,
                           typeSymbol: TypeSymbol,
                           isReadOnly: Boolean): VariableSymbol = {
    val name =
      if (identifier.text == null || identifier.text == "") "?"
      else identifier.text
    val declare = !identifier.isMissing
    val variable = new VariableSymbol(name, typeSymbol, isReadOnly)
    if (declare && !scope.tryDeclare(variable))
      diagnostics.reportFunctionAlreadyDeclared(identifier.span, name)
    variable
  }

  private def bindBlockStatement(
      statement: BlockStatement): BindBlockStatement = {
    var statements: List[BindStatement] = List()
    scope = BindScope(this.scope)
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
    val name = node.identifierToken.text
    if (name == null || name.isEmpty)
      return BindLiteralExpression(0)
    val variable = scope.tryLookupVariable(name)
    if (variable == null) {
      diagnostics.reportUndefinedName(node.identifierToken.span, name)
      return BindLiteralExpression(0)
    }
    BindVariableExpression(variable)
  }

  private def bindAssignmentExpression(node: AssignmentNode): BindExpression = {
    val name = node.identifierToken.text
    val boundExpression = bindExpression(node.expression)
    val existingVariable = scope.tryLookupVariable(name)
    if (existingVariable == null) {
      diagnostics.reportUndefinedName(node.identifierToken.span, name)
      return boundExpression
    }
    if (existingVariable.isReadOnly)
      diagnostics.reportCannotAssign(node.equalsToken.span, name)

    if (boundExpression.getType != existingVariable.typeSymbol) {
      diagnostics.reportCannotConvert(node.equalsToken.span,
                                      boundExpression.getType,
                                      existingVariable.typeSymbol)
      return boundExpression
    }
    BindAssignmentExpression(existingVariable, boundExpression)
  }

  private def bindLiteralExpression(node: LiteralNode): BindExpression = {
    BindLiteralExpression(node.value.value)
  }

  private def bindBinaryExpression(node: BinaryNode): BindExpression = {
    val boundLeft = bindExpression(node.left)
    val boundRight = bindExpression(node.right)
    val boundOperator =
      BoundBinaryOperator.bind(
        node.op.tokenType,
        boundLeft.getType,
        boundRight.getType
      )
    if (boundOperator == null) {
      diagnostics.reportUndefinedBinaryOperator(
        node.op.span,
        node.op.text,
        boundLeft.getType.name,
        boundRight.getType.name
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
        boundOperand.getType
      )
    if (boundOperatorKind == null) {
      diagnostics.reportUndefinedUnaryOperator(
        node.op.asInstanceOf[Token].span,
        node.op.asInstanceOf[Token].text,
        boundOperand.getType.name
      )
      return boundOperand
    }
    BindUnaryExpression(boundOperatorKind, boundOperand)
  }
}

object Binder {
  def lookupType(name: String): TypeSymbol = {
    name match {
      case "int"    => TypeSymbol.Int
      case "bool"   => TypeSymbol.Bool
      case "double" => TypeSymbol.Double
      case "string" => TypeSymbol.String
      case _        => null
    }
  }

  def bindGlobalScope(previous: BoundGlobalScope,
                      syntax: CompilationUnit): BoundGlobalScope = {
    val parentScope = createParentScope(previous)
    val binder = Binder(parentScope,null)
    for(function <- syntax.members){
      binder.bindFunctionDeclaration(function.asInstanceOf[FunctionDeclarationNode])
    }
    val statements = ListBuffer[BindStatement]()
    for(globalStatement <- syntax.members){
      val statement = binder.bindStatement(globalStatement.asInstanceOf[GlobalStatementNode].statement)
      statements += statement
    }
    val variables = binder.scope.getDeclaredVariables
    val functions = binder.scope.getDeclaredFunctions
    val diagnostics = binder.diagnostics
    if (previous != null)
      diagnostics concat previous.diagnostics
    BoundGlobalScope(previous, diagnostics, variables, functions, statements.toList)
  }

  def createParentScope(previous: BoundGlobalScope): BindScope = {
    var pre = previous
    val stack = mutable.Stack[BoundGlobalScope]()
    while (pre != null) {
      stack.push(pre)
      pre = pre.previous
    }
    var parent: BindScope = createRootScope()
    while (stack.nonEmpty) {
      pre = stack.pop()
      val scope = BindScope(parent)
      for (v <- pre.variables)
        scope.tryDeclare(v)
      parent = scope
    }
    parent
  }


  private def createRootScope(): BindScope = {
    val result = BindScope(null)
    BuiltinFunctions.getAll.foreach(result.tryDeclare)
    result
  }
}

abstract class BoundNode {
  def getKind: BindType.BindType
}

abstract class BindExpression extends BoundNode {
  def getType: TypeSymbol
}

abstract class BindStatement extends BoundNode {}

case class BindExpressionStatement(bindExpression: BindExpression)
    extends BindStatement {

  override def getKind: BindType.BindType = BindType.expressionStatement
}

case class BindBlockStatement(bindStatements: List[BindStatement])
    extends BindStatement {

  override def getKind: BindType.BindType = BindType.blockStatement
}

case class BindVariableStatement(variableSymbol: VariableSymbol,
                                 initializer: BindExpression)
    extends BindStatement {

  override def getKind: BindType = BindType.variableDeclaration
}

case class BindIfStatement(condition: BindExpression,
                           expr1: BindStatement,
                           expr2: BindStatement)
    extends BindStatement {
  override def getKind: BindType = BindType.ifStatement

}

case class BindWhileStatement(condition: BindExpression, body: BindStatement)
    extends BindStatement {
  override def getKind: BindType = BindType.whileStatement

}

case class BindForStatement(variable: VariableSymbol,
                            initializer: BindExpression,
                            upper: BindExpression,
                            body: BindStatement)
    extends BindStatement {
  override def getKind: BindType = BindType.forStatement

}

case class BindFuncStatement(identifier: VariableSymbol,
                             param: List[VariableSymbol],
                             body: BindStatement)
    extends BindStatement {
  override def getKind: BindType = BindType.funcStatement

}

case class BindBinaryExpression(bindType: BoundBinaryOperator,
                                boundLeft: BindExpression,
                                boundRight: BindExpression)
    extends BindExpression {
  override def getType: TypeSymbol = bindType.result

  override def getKind: BindType = bindType.bindType
}

case class BindUnaryExpression(bindType: BoundUnaryOperator,
                               boundOperand: BindExpression)
    extends BindExpression {
  override def getType: TypeSymbol = boundOperand.getType
  override def getKind: BindType = bindType.bindType
}

case class BindLiteralExpression(value: Any) extends BindExpression {
  override def getType: TypeSymbol = {
    value match {
      case _: Boolean => TypeSymbol.Bool
      case _: Int     => TypeSymbol.Int
      case _: Double  => TypeSymbol.Double
      case _: String  => TypeSymbol.String
      case _ =>
        throw new Exception(
          s"Unexpected literal '$value' of type ${value.getClass}");

    }
  }

  override def getKind: BindType = BindType.literalExpression
}

sealed class BoundBinaryOperator(val tokenType: TokenType,
                                 val bindType: BindType,
                                 val left: TypeSymbol,
                                 val right: TypeSymbol,
                                 val result: TypeSymbol)

case class BindVariableExpression(variableSymbol: VariableSymbol)
    extends BindExpression {
  override def getType: TypeSymbol = variableSymbol.typeSymbol

  override def getKind: BindType = BindType.variableDeclaration
}

case class BindAssignmentExpression(variable: VariableSymbol,
                                    expression: BindExpression)
    extends BindExpression {
  override def getType: TypeSymbol = expression.getType

  override def getKind: BindType = BindType.assignmentExpression
}

case class BindFuncCallExpression(functionSymbol: FunctionSymbol,
                                  paramList: List[BindExpression])
    extends BindExpression {
  override def getKind: BindType = BindType.funcStatement

  override def getType: TypeSymbol = functionSymbol.typeSymbol
}

case class BindErrorExpression() extends BindExpression {
  override def getType: TypeSymbol = TypeSymbol.Error

  override def getKind: BindType = BindType.errorExpression
}

case class BindConversionExpression(typeSymbol: TypeSymbol,
                                    bindExpression: BindExpression)
    extends BindExpression {
  override def getType: TypeSymbol = typeSymbol

  override def getKind: BindType = BindType.conversionExpression
}

object BoundBinaryOperator {

  def apply(
      tokenType: TokenType,
      bindType: BindType,
      left: TypeSymbol,
      right: TypeSymbol,
      result: TypeSymbol
  ): BoundBinaryOperator =
    new BoundBinaryOperator(
      tokenType,
      bindType,
      left,
      right,
      result
    )

  def bind(tokenType: TokenType,
           left: TypeSymbol,
           right: TypeSymbol): BoundBinaryOperator = {
    val bindType = getBindType(tokenType)

    if (left == right) {
      if (comparableOperations.contains(bindType))
        BoundBinaryOperator(tokenType,
                            getBindType(tokenType),
                            left,
                            right,
                            TypeSymbol.Bool)
      else if (computeOperations.contains(bindType))
        BoundBinaryOperator(tokenType,
                            getBindType(tokenType),
                            left,
                            right,
                            left)
      else if (logicOperations.contains(bindType))
        BoundBinaryOperator(tokenType,
                            getBindType(tokenType),
                            TypeSymbol.Bool,
                            TypeSymbol.Bool,
                            TypeSymbol.Bool)
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

  def convert(left: TypeSymbol, right: TypeSymbol): Boolean =
    (left, right) match {
      case (TypeSymbol.Int, TypeSymbol.Double) => true
      case _                                   => false
    }

  def getBindType(tokenType: TokenType): BindType.Value = {
    tokenType match {
      case TokenType.`plusToken`            => BindType.addition
      case TokenType.`minusToken`           => BindType.subtraction
      case TokenType.`starToken`            => BindType.multiplication
      case TokenType.`slashToken`           => BindType.division
      case TokenType.`hatToken`             => BindType.pow
      case TokenType.`modToken`             => BindType.mod
      case TokenType.`lessToken`            => BindType.lt
      case TokenType.`greaterToken`         => BindType.gt
      case TokenType.`lessOrEqualsToken`    => BindType.lte
      case TokenType.`greaterOrEqualsToken` => BindType.gte
      case TokenType.`equalsEqualsToken`    => BindType.equal
      case TokenType.`bangEqualsToken`      => BindType.notequal
      case TokenType.ampersandToken         => BindType.and
      case TokenType.`pipeToken`            => BindType.or
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
                                val operand: TypeSymbol,
                                val result: TypeSymbol)

object BoundUnaryOperator {

  def apply(
      tokenType: TokenType,
      bindType: BindType,
      operand: TypeSymbol,
      result: TypeSymbol
  ): BoundUnaryOperator = new BoundUnaryOperator(
    tokenType,
    bindType,
    operand,
    result
  )

  private[this] def unaryOperators: List[BoundUnaryOperator] =
    List(
      BoundUnaryOperator(TokenType.tildeToken,
                         BindType.not,
                         TypeSymbol.Bool,
                         TypeSymbol.Bool),
      BoundUnaryOperator(TokenType.minusToken,
                         BindType.negation,
                         TypeSymbol.Double,
                         TypeSymbol.Double),
      BoundUnaryOperator(TokenType.minusToken,
                         BindType.negation,
                         TypeSymbol.Int,
                         TypeSymbol.Int),
      BoundUnaryOperator(TokenType.plusToken,
                         BindType.identity,
                         TypeSymbol.Double,
                         TypeSymbol.Double),
      BoundUnaryOperator(TokenType.plusToken,
                         BindType.identity,
                         TypeSymbol.Int,
                         TypeSymbol.Int)
    )

  def bind(tokenType: TokenType, operand: TypeSymbol): BoundUnaryOperator = {
    val unaryOperator = unaryOperators.filter(x =>
      x.tokenType == tokenType && x.operand == operand)
    if (unaryOperator.nonEmpty)
      unaryOperator.last
    else
      null
  }

}
