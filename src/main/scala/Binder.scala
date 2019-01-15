import BindType.BindType
import TokenType.TokenType
import TypeMapping._

import scala.collection.mutable

case class Binder(parent:BoundScope) {
  val diagnostics: DiagnosticsBag = DiagnosticsBag()
  var scope:BoundScope = BoundScope(parent)


  def bindExpression(tree: Expression): BindExpression = {
    tree.getKind match {
      case TokenType.nameExpression =>
        bindNameExpression(tree.asInstanceOf[NameNode])
      case TokenType.assignmentExpression =>
        bindAssignmentExpression(tree.asInstanceOf[AssignmentNode])
      case TokenType.binaryExpression =>
        bindBinaryExpression(tree.asInstanceOf[BinaryNode])
      case TokenType.unaryExpression =>
        bindUnaryExpression(tree.asInstanceOf[UnaryNode])
      case TokenType.numberExpression =>
        bindLiteralExpression(tree.asInstanceOf[LiteralNode])
      case TokenType.`compilationUnit` =>
        bindExpression(tree.asInstanceOf[CompilationUnit].expr)
      case TokenType.braceExpression =>
        bindExpression(tree.asInstanceOf[BraceNode].op)
      case _ =>
        throw new LexerException(s"unexpected syntax ${tree.getKind}")
    }
  }

  private def bindNameExpression(node: NameNode): BindExpression = {
    val name = node.identifierToken.value
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
    var existingVariable = scope.tryLookup(name)
    if(existingVariable == null){
      existingVariable = VariableSymbol(name,boundExpression.bindTypeClass)
      scope.tryDeclare(existingVariable)
    }

    if(boundExpression.bindTypeClass != existingVariable.varType){
      diagnostics.reportCannotConvert(node.expression.asInstanceOf[Tokens].span,boundExpression.bindTypeClass,existingVariable.varType)
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
    val boundOperand = bindExpression(node.oprand)
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
  def bindGlobalScope(previous:BoundGlobalScope,
                      syntax:CompilationUnit): BoundGlobalScope = {
    val parentScope = createParentScope(previous)
    val binder = Binder(parentScope)
    val expression = binder.bindExpression(syntax.expr)
    val variables = binder.scope.getDeclaredVariables
    val diagnostics = binder.diagnostics
    if(previous != null)
      diagnostics concat previous.diagnostics
    BoundGlobalScope(previous,diagnostics,variables,expression)
  }

  def createParentScope(previous:BoundGlobalScope): BoundScope ={
    var pre = previous
    val stack = mutable.Stack[BoundGlobalScope]()
    while(pre != null){
      stack.push(pre)
      pre = pre.previous
    }
    var parent:BoundScope = null
    while(stack.nonEmpty){
      pre = stack.pop()
      val scope = BoundScope(parent)
      for(v <- pre.variables)
        scope.tryDeclare(v)
      parent = scope
    }
    parent
  }
}



abstract class BoundNode {
  def bindTypeClass: String
}

abstract class BindExpression extends BoundNode {

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


