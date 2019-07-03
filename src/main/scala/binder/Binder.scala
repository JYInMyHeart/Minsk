package binder

import eval.DiagnosticsBag
import lowering.Lowerer
import parser.BindType.BindType
import parser.TokenType.TokenType
import parser._
import sourceText.TextSpan
import symbol._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Binder(parent: BindScope, function: FunctionSymbol) {

  private var labelCounter: Int = _
  private val loopStack: mutable.Stack[(BindLabel, BindLabel)] = mutable.Stack()
  def lookupTyoe(name: String): TypeSymbol = name match {
    case "bool" =>
      TypeSymbol.Bool
    case "int" =>
      TypeSymbol.Int
    case "string" =>
      TypeSymbol.String
    case "void" =>
      TypeSymbol.Void
    case "double" =>
      TypeSymbol.Double
    case _ => null
  }

  def bindTypeClause(node: TypeClauseNode): TypeSymbol = {
    if (node == null) return null
    val functionType = lookupTyoe(node.identifier.text)
    if (functionType == null)
      diagnostics.reportUndefinedType(node.identifier.text,
                                      node.identifier.getSpan)
    functionType
  }

  def bindFunctionDeclaration(node: FunctionDeclarationNode): Unit = {
    val parameters = ListBuffer[ParameterSymbol]()
    val seenParameterNames = mutable.HashSet[String]()
    for (parameterNode <- node.parameters) {
      val parameterName = parameterNode.identifier.text
      val parameterType = bindTypeClause(parameterNode.parameterType)
      if (!seenParameterNames.add(parameterName)) {
        diagnostics.reportParameterAlreadyDeclared(parameterNode.getSpan,
                                                   parameterName)
      } else {
        val parameter = ParameterSymbol(parameterName, parameterType)
        parameters += parameter
      }
    }
    val bindType = bindTypeClause(node.functionType)
    val functionType = if (bindType == null) TypeSymbol.Void else bindType
    val declaredFunction =
      new FunctionSymbol(node.identifier.text,
                         parameters.toList,
                         functionType,
                         node)
    if (!scope.tryDeclareFunction(declaredFunction))
      diagnostics.reportFunctionAlreadyDeclared(node.identifier.span,
                                                declaredFunction.name)
  }

  val diagnostics: DiagnosticsBag = DiagnosticsBag()
  var scope: BindScope = BindScope(parent)
  var _function: FunctionSymbol = initFunction

  def initFunction: FunctionSymbol = {
    if (function != null)
      for (p <- function.parameters)
        scope.tryDeclareVariable(p)
    function
  }

  def bindReturnStatement(statement: ReturnStatement): BindStatement = {
    var expression =
      if (statement.expression == null) null
      else bindExpression(statement.expression)
    if (_function == null)
      diagnostics.reportInvalidReturn(statement.keyword.span)
    else {
      if (_function.typeSymbol == TypeSymbol.Void)
        if (expression != null)
          diagnostics.reportInvalidReturnExpression(
            statement.expression.getSpan,
            _function.name)
        else {
          if (expression == null)
            diagnostics.reportMissingReturnExpression(statement.keyword.span,
                                                      _function.typeSymbol)
          else
            expression = bindConversionIn(statement.expression.getSpan,
                                          _function.typeSymbol,
                                          expression)
        }
    }
    BindReturnStatement(expression)
  }

  def bindBreakStatement(statement: BreakStatement): BindStatement = {
    if (loopStack.isEmpty) {
      diagnostics.reportInvalidBreakOrContinue(statement.keyword.getSpan,
                                               statement.keyword.text)
      return bindErrorStatement()
    }
    val breakLabel = loopStack.head._1
    BindGotoStatement(breakLabel)
  }

  def bindErrorStatement() = BindExpressionStatement(BindErrorExpression())

  def bindContinueStatement(statement: ContinueStatement): BindStatement = {
    if (loopStack.isEmpty) {
      diagnostics.reportInvalidBreakOrContinue(statement.keyword.getSpan,
                                               statement.keyword.text)
      return bindErrorStatement()
    }
    val continueLabel = loopStack.head._2
    BindGotoStatement(continueLabel)
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
      case (TokenType.returnStatement, s: ReturnStatement) =>
        bindReturnStatement(s)
      case (TokenType.breakStatement, s: BreakStatement) =>
        bindBreakStatement(s)
      case (TokenType.continueStatement, s: ContinueStatement) =>
        bindContinueStatement(s)
      case _ =>
        throw new Exception(s"unexpected syntax ${statement.getKind}")
    }
  }

//  def bindExpression(expression: Expression,
//                     targetType: TypeSymbol): BindExpression = {
//    val res = bindExpression(expression)
//    if (res.getType != targetType)
//      diagnostics.reportCannotConvert(null, res.getType, targetType)
//    res
//  }

  def bindConversion(node: Expression,
                     typeSymbol: TypeSymbol,
                     allowExplicit: Boolean = false): BindExpression = {
    val expression = bindExpression(node)
    bindConversionIn(node.getSpan, typeSymbol, expression, allowExplicit)
  }
  def bindConversionIn(diagnosticSpan: TextSpan,
                       typeSymbol: TypeSymbol,
                       expression: BindExpression,
                       allowExplicit: Boolean = false): BindExpression = {
    val conversion = Conversion.classify(expression.getType, typeSymbol)
    if (!conversion.exists) {
      if (expression.getType != TypeSymbol.Error && typeSymbol != TypeSymbol.Error)
        diagnostics.reportCannotConvert(diagnosticSpan,
                                        expression.getType,
                                        typeSymbol)
      return BindErrorExpression()
    }
    BindConversionExpression(typeSymbol, expression)
  }

  def bindFuncCallExpression(n: FunctionCallNode): BindExpression = {
    val typeSymbol = Binder.lookupType(n.identifier.text)
    if (n.arguments.length == 1 && typeSymbol != null) {
      bindConversion(n.arguments.head, typeSymbol, allowExplicit = true)
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

  def bindExpression(tree: Expression, targetType: TypeSymbol): BindExpression =
    bindConversion(tree, targetType)

  def bindExpression(tree: Expression, canBeVoid: Boolean): BindExpression = {
    val result = bindExpression(tree)
    if (!canBeVoid && result.getType == TypeSymbol.Void) {
      diagnostics.reportExpressionMustHaveValue(tree.getSpan)
      BindErrorExpression()
    }
    result
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

  def bindLoopBody(body: Statement): (BindStatement, BindLabel, BindLabel) = {
    labelCounter += 1
    val breakLabel = BindLabel(s"break$labelCounter")
    val continueLabel = BindLabel(s"continue$labelCounter")
    loopStack.push((breakLabel, continueLabel))
    val boundBody = bindStatement(body)
    loopStack.pop()
    (boundBody, breakLabel, continueLabel)
  }

  private def bindForStatement(statement: ForStatement): BindForStatement = {
    val low = bindExpression(statement.low, TypeSymbol.Int)
    val upper = bindExpression(statement.upper, TypeSymbol.Int)
    scope = BindScope(scope)
    val variableSymbol =
      bindVariable(statement.identifier, TypeSymbol.Int, isReadOnly = true)

    val (body, breakLabel, continueLabel) = bindLoopBody(statement.body)
    scope = scope.parent
    BindForStatement(variableSymbol,
                     low,
                     upper,
                     body,
                     breakLabel,
                     continueLabel)
  }

  private def bindWhileStatement(
      statement: WhileStatement): BindWhileStatement = {
    val condition = bindExpression(statement.condition, TypeSymbol.Bool)
    val (body, break, continue) = bindLoopBody(statement.body)
    BindWhileStatement(condition, body, break, continue)
  }

  private def bindIfStatement(statement: IfStatement): BindIfStatement = {
    val condition = bindExpression(statement.condition, TypeSymbol.Bool)
    val thenStatement = bindStatement(statement.expr1)
    val elseStatement =
      if (statement.expr2 == null) null else bindStatement(statement.expr2)
    BindIfStatement(condition, thenStatement, elseStatement)

  }

  private def bindVariableDeclaration(
      statement: VariableDeclarationNode): BindVariableDeclaration = {
    val name = statement.identifier
    val isReadOnly = statement.keyword.tokenType == TokenType.letKeyword
    val initializer = bindExpression(statement.expression)
    val variable = bindVariable(name, initializer.getType, isReadOnly)
    BindVariableDeclaration(variable, initializer)
  }

  private def bindVariable(identifier: Token,
                           typeSymbol: TypeSymbol,
                           isReadOnly: Boolean): VariableSymbol = {
    val name =
      if (identifier.text == null || identifier.text == "") "?"
      else identifier.text
    val declare = !identifier.isMissing
    val variable =
      if (_function == null)
        new GlobalVariableSymbol(name, typeSymbol, isReadOnly)
      else new LocalVariableSymbol(name, typeSymbol, isReadOnly)
    if (declare && !scope.tryDeclareVariable(variable))
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

  def bindProgram(globalScope: BoundGlobalScope): BindProgram = {
    val parentScope = createParentScope(globalScope)
    val functionBodies = mutable.HashMap[FunctionSymbol, BindBlockStatement]()
    val diagnostics = DiagnosticsBag()
    var scope = globalScope
    while (scope != null) {
      for (function <- scope.functions) {
        val binder = Binder(parentScope, function)
        val body = binder.bindStatement(function.functionDeclarationNode.body)
        val loweredBody = Lowerer.lower(body)
//        if(function.typeSymbol != TypeSymbol.Void )
        functionBodies += function -> loweredBody
        diagnostics concat binder.diagnostics
      }
      scope = scope.previous
    }
    val statement = Lowerer.lower(BindBlockStatement(globalScope.statement))
    BindProgram(diagnostics, functionBodies, statement)
  }

  def bindGlobalScope(previous: BoundGlobalScope,
                      syntax: CompilationUnit): BoundGlobalScope = {
    val parentScope = createParentScope(previous)
    val binder = Binder(parentScope, null)
    for (function <- syntax.members.filter(
           _.isInstanceOf[FunctionDeclarationNode])) {
      binder.bindFunctionDeclaration(
        function.asInstanceOf[FunctionDeclarationNode])
    }
    val statements = ListBuffer[BindStatement]()
    for (globalStatement <- syntax.members.filter(
           _.isInstanceOf[GlobalStatementNode])) {
      val statement = binder.bindStatement(
        globalStatement.asInstanceOf[GlobalStatementNode].statement)
      statements += statement
    }
    val variables = binder.scope.getDeclaredVariables
    val functions = binder.scope.getDeclaredFunctions
    val diagnostics = binder.diagnostics
    if (previous != null)
      diagnostics concat previous.diagnostics
    BoundGlobalScope(previous,
                     diagnostics,
                     variables,
                     functions,
                     statements.toList)
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
        scope.tryDeclareVariable(v)
      for (f <- pre.functions)
        scope.tryDeclareFunction(f)
      parent = scope
    }
    parent
  }

  private def createRootScope(): BindScope = {
    val result = BindScope(null)
    BuiltinFunctions.getAll.foreach(result.tryDeclareFunction)
    result
  }
}

abstract class BindTreeRewriter {

  def rewriteBindVariableStatement(
      n: BindVariableDeclaration): BindStatement = {
    val initializer = rewriteExpression(n.initializer)
    if (initializer == n.initializer)
      return n
    BindVariableDeclaration(n.variableSymbol, initializer)
  }

  def rewriteBlockStatement(n: BindBlockStatement): BindStatement = {
    var builders: ListBuffer[BindStatement] = null
    for (i <- n.bindStatements.indices) {
      val oldStatement = n.bindStatements(i)
      val newStatement = rewriteStatement(oldStatement)
      if (newStatement != oldStatement) {
        if (builders == null) {
          builders = ListBuffer[BindStatement]()
          for (j <- 0 until i)
            builders += n.bindStatements(j)
        }
      }
      if (builders != null)
        builders += newStatement
    }
    if (builders == null)
      return n
    BindBlockStatement(builders.toList)
  }

  def rewriteBindIfStatement(n: BindIfStatement): BindStatement = {
    val condition = rewriteExpression(n.condition)
    val thenStatement = rewriteStatement(n.thenStatement)
    val elseStatement =
      if (n.elseStatement == null) null else rewriteStatement(n.elseStatement)
    if (condition == n.condition && thenStatement == n.thenStatement && elseStatement == n.thenStatement)
      return n
    BindIfStatement(condition, thenStatement, elseStatement)
  }

  def rewriteBindWhileStatement(n: BindWhileStatement): BindStatement = {
    val condition = rewriteExpression(n.condition)
    val body = rewriteStatement(n.body)
    if (condition == n.condition && body == n.body)
      return n
    BindWhileStatement(condition, body, n.breakLabel, n.continueLabel)
  }

  def rewriteBindForStatement(n: BindForStatement): BindStatement = {
    val variable = n.variable
    val initializer = rewriteExpression(n.initializer)
    val upper = rewriteExpression(n.upper)
    val body = rewriteStatement(n.body)
    if (initializer == n.initializer && upper == n.upper && body == n.body)
      return n
    BindForStatement(variable,
                     initializer,
                     upper,
                     body,
                     n.breakLabel,
                     n.continueLabel)
  }

  def rewriteErrorExpression(node: BindErrorExpression): BindExpression = node

  def rewriteLiteralExpression(node: BindLiteralExpression): BindExpression =
    node

  def rewriteVariableExpression(node: BindVariableExpression): BindExpression =
    node

  def rewriteAssignmentExpression(
      node: BindAssignmentExpression): BindExpression = {
    val expression = rewriteExpression(node.expression)
    if (expression == node.expression)
      return node
    BindAssignmentExpression(node.variable, expression)
  }

  def rewriteBinaryExpression(node: BindBinaryExpression): BindExpression = {
    val left = rewriteExpression(node.boundLeft)
    val right = rewriteExpression(node.boundRight)
    if (left == node.boundLeft && right == node.boundRight)
      return node
    BindBinaryExpression(node.operator, left, right)
  }

  def rewriteFuncCallExpression(
      node: BindFuncCallExpression): BindExpression = {
    val builder = ListBuffer[BindExpression]()
    for (i <- node.paramList.indices) {
      val oldArguement = node.paramList(i)
      val newArguement = rewriteExpression(oldArguement)
      if (oldArguement != newArguement) {
        if (builder.isEmpty) {
          for (j <- 0 until i)
            builder += node.paramList(j)
        }
      }
      if (builder.nonEmpty)
        builder += newArguement
    }
    if (builder.isEmpty)
      return node
    BindFuncCallExpression(node.functionSymbol, builder.toList)
  }

  def rewriteUnaryExpression(node: BindUnaryExpression): BindExpression = {
    val operand = rewriteExpression(node.boundOperand)
    if (operand == node.boundOperand)
      return node
    BindUnaryExpression(node.bindType, operand)
  }

  def rewriteConversionExpression(
      node: BindConversionExpression): BindExpression = {
    val expression = rewriteExpression(node.bindExpression)
    if (expression == node.bindExpression)
      return node
    BindConversionExpression(node.typeSymbol, expression)
  }

  def rewriteExpression(bindExpression: BindExpression): BindExpression = {
    bindExpression match {
      case node: BindErrorExpression      => rewriteErrorExpression(node)
      case node: BindLiteralExpression    => rewriteLiteralExpression(node)
      case node: BindVariableExpression   => rewriteVariableExpression(node)
      case node: BindAssignmentExpression => rewriteAssignmentExpression(node)
      case node: BindBinaryExpression     => rewriteBinaryExpression(node)
      case node: BindFuncCallExpression   => rewriteFuncCallExpression(node)
      case node: BindConversionExpression => rewriteConversionExpression(node)
      case node: BindUnaryExpression      => rewriteUnaryExpression(node)
      case _ =>
        throw new Exception(s"Unexpected node: ${bindExpression.getKind}")
    }
  }

  def rewriteBindExpressionStatement(
      n: BindExpressionStatement): BindStatement = {
    val bindExpression = rewriteExpression(n.bindExpression)
    if (bindExpression == n.bindExpression)
      return n
    BindExpressionStatement(bindExpression)
  }

  def rewriteBindLabelStatement(n: BindLabelStatement): BindStatement = n

  def rewriteBindGotoStatementt(n: BindGotoStatement): BindStatement = n

  def rewriteBindConditionGotoStatementt(
      n: BindConditionGotoStatement): BindStatement = {
    val condition = rewriteExpression(n.condition)
    if (condition == n.condition)
      return n
    BindConditionGotoStatement(n.label, condition, n.jumpIfTrue)
  }

  def rewriteBindReturnStatement(n: BindReturnStatement): BindStatement = {
    val expression =
      if (n.expression == null) null else rewriteExpression(n.expression)
    if (expression == n.expression)
      return n
    BindReturnStatement(expression)
  }

  def rewriteStatement(node: BindStatement): BindStatement = {
    node match {
      case n: BindBlockStatement      => rewriteBlockStatement(n)
      case n: BindVariableDeclaration => rewriteBindVariableStatement(n)
      case n: BindIfStatement         => rewriteBindIfStatement(n)
      case n: BindWhileStatement      => rewriteBindWhileStatement(n)
      case n: BindForStatement        => rewriteBindForStatement(n)
      case n: BindExpressionStatement => rewriteBindExpressionStatement(n)
      case n: BindLabelStatement      => rewriteBindLabelStatement(n)
      case n: BindGotoStatement       => rewriteBindGotoStatementt(n)
      case n: BindConditionGotoStatement =>
        rewriteBindConditionGotoStatementt(n)
      case n: BindReturnStatement => rewriteBindReturnStatement(n)
      case _ =>
        throw new Exception(s"unexpected node:${node.getKind}")
    }
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

case class BindVariableDeclaration(variableSymbol: VariableSymbol,
                                   initializer: BindExpression)
    extends BindStatement {

  override def getKind: BindType = BindType.variableDeclaration
}

case class BindIfStatement(condition: BindExpression,
                           thenStatement: BindStatement,
                           elseStatement: BindStatement)
    extends BindStatement {
  override def getKind: BindType = BindType.ifStatement

}

case class BindWhileStatement(condition: BindExpression,
                              body: BindStatement,
                              breakLabel: BindLabel,
                              continueLabel: BindLabel)
    extends BindLoopStatement(breakLabel, continueLabel) {
  override def getKind: BindType = BindType.whileStatement

}

case class BindForStatement(variable: VariableSymbol,
                            initializer: BindExpression,
                            upper: BindExpression,
                            body: BindStatement,
                            breakLabel: BindLabel,
                            continueLabel: BindLabel)
    extends BindLoopStatement(breakLabel, continueLabel) {
  override def getKind: BindType = BindType.forStatement

}

abstract class BindLoopStatement(breakLabel: BindLabel,
                                 continueLabel: BindLabel)
    extends BindStatement

case class BindFuncStatement(identifier: VariableSymbol,
                             param: List[VariableSymbol],
                             body: BindStatement)
    extends BindStatement {
  override def getKind: BindType = BindType.funcStatement

}

case class BindGotoStatement(label: BindLabel) extends BindStatement {
  override def getKind: BindType = BindType.gotoStatement
}

case class BindConditionGotoStatement(label: BindLabel,
                                      condition: BindExpression,
                                      jumpIfTrue: Boolean = true)
    extends BindStatement {
  override def getKind: BindType = BindType.conditionGotoStatement
}

case class BindReturnStatement(expression: BindExpression)
    extends BindStatement {
  override def getKind: BindType = BindType.returnStatement
}

case class BindBinaryExpression(operator: BoundBinaryOperator,
                                boundLeft: BindExpression,
                                boundRight: BindExpression)
    extends BindExpression {
  override def getType: TypeSymbol = operator.result

  override def getKind: BindType = operator.bindType
}

case class BindUnaryExpression(bindType: BoundUnaryOperator,
                               boundOperand: BindExpression)
    extends BindExpression {
  override def getType: TypeSymbol = boundOperand.getType
  override def getKind: BindType = bindType.bindType
}

case class BindLabel(name: String) {
  override def toString: String = name
}

case class BindLabelStatement(label: BindLabel) extends BindStatement {
  override def getKind: BindType = BindType.labelStatement
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

case class BindProgram(
    diagnostics: DiagnosticsBag,
    functions: mutable.HashMap[FunctionSymbol, BindBlockStatement],
    statement: BindBlockStatement) {}

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
