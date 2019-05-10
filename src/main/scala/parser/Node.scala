package parser

import eval.DiagnosticsBag
import parser.TokenType.TokenType
import sourceText.{SourceText, TextSpan}

import scala.collection.mutable.ListBuffer

abstract class Node {
  def getKind: TokenType.TokenType

  def getSpan: TextSpan = {
    val first = getChildren.head.getSpan
    val last = getChildren.last.getSpan
    TextSpan.fromBounds(first.start, last.end)
  }
  def getChildren:List[Node] = {
    val result = ListBuffer[Node]()
    val properties = getClass.getDeclaredFields
    for (property <- properties) {
      if (classOf[Node].isAssignableFrom(property.getType)) {
        val child = property.get().asInstanceOf[Node]
        if (child != null)
          result += child
      } else if (classOf[List[Node]].isAssignableFrom(property.getType)) {
        val child = property.get().asInstanceOf[List[Node]]
        if (child != null)
          result ++= child
      }
    }
    result.toList
  }

  def getLastToken: Token = {
    this match {
      case token: Token => token
      case _            => this.getChildren.last.getLastToken
    }

  }
}

abstract class Expression extends Node

abstract class Statement extends Node

case class ExpressionStatement(expression: Expression) extends Statement {
  override def getKind: TokenType = TokenType.expressionStatement

  override def getChildren: List[Expression] = List(expression)
}

case class BlockStatement(openBraceToken: Token,
                          statements: List[Statement],
                          closeBraceToken: Token)
    extends Statement {
  override def getKind: TokenType = TokenType.blockStatement

  override def getChildren: List[Node] = statements
}

case class FuncStatement(funcToken: Token,
                         identifier: Token,
                         parameters: List[ParamStatement],
                         returnType: ParamStatement,
                         body: Statement)
    extends Statement {
  override def getKind: TokenType = TokenType.funcStatement

  override def getChildren: List[Node] =
    List(
      funcToken,
      identifier
    ) ++ parameters :+ returnType :+ body
}

case class ParamStatement(id: Token, paramType: Token) extends Statement {
  override def getKind: TokenType = TokenType.paramStatement

  override def getChildren: List[Node] = List(
    id,
    paramType
  )
}

case class IfStatement(ifToken: Token,
                       condition: Expression,
                       expr1: Statement,
                       elseToken: Token,
                       expr2: Statement)
    extends Statement {
  override def getKind: TokenType = TokenType.ifStatement

  override def getChildren: List[Node] =
    List(
      ifToken,
      condition,
      expr1,
      elseToken,
      expr2
    )
}

case class ForStatement(forToken: Token,
                        identifier: Token,
                        equalToken: Token,
                        low: Expression,
                        toToken: Token,
                        upper: Expression,
                        body: Statement)
    extends Statement {
  override def getKind: TokenType = TokenType.forStatement

  override def getChildren: List[Node] =
    List(
      forToken,
      identifier,
      equalToken,
      low,
      toToken,
      upper,
      body
    )
}

case class WhileStatement(whileToken: Token,
                          condition: Expression,
                          body: Statement)
    extends Statement {
  override def getKind: TokenType = TokenType.whileStatement

  override def getChildren: List[Node] =
    List(
      whileToken,
      condition,
      body
    )
}

case class SyntaxTree(diagnostics: DiagnosticsBag, root: CompilationUnit)

object SyntaxTree {
  def parse(text: String): SyntaxTree = {
    val sourceText = SourceText(text)
    val parser = new Parser(sourceText)
    parser.init()
    val expr = parser.parseCompilationUnit()
    SyntaxTree(parser.diagnostics, CompilationUnit(expr, TokenType.eofToken))
  }
}

case class CompilationUnit(statement: Statement, eof: TokenType)
    extends Statement {
  override def getKind: TokenType.TokenType = TokenType.compilationUnit

  override def toString: String = s"ExpressionTree:$statement"
}

case class BinaryNode(left: Expression, op: Token, right: Expression)
    extends Expression {
  override def getKind: TokenType.TokenType = TokenType.binaryExpression

  override def getChildren: List[Expression] = {
    List[Expression](left, op, right)
  }

  override def toString: String = s"parser.BinaryNode:${left.getKind}"
}

case class LiteralNode(value: Token) extends Expression {
  override def getKind: TokenType.TokenType = TokenType.numberExpression

  override def getChildren: List[Expression] = {
    List[Expression](value)
  }
}

case class BraceNode(left: Expression, op: Expression, right: Expression)
    extends Expression {
  override def getKind: TokenType.TokenType = TokenType.braceExpression

  override def getChildren: List[Expression] = List(left, op, right)

}

case class UnaryNode(op: Expression, operand: Expression) extends Expression {
  override def getKind: TokenType.TokenType = TokenType.unaryExpression

  override def getChildren: List[Expression] = List[Expression](op, operand)
}

case class NameNode(identifierToken: Token) extends Expression {
  override def getKind: TokenType.TokenType = TokenType.nameExpression

  override def getChildren: List[Expression] = List(identifierToken)
}

case class AssignmentNode(identifierToken: Token,
                          equalsToken: Token,
                          expression: Expression)
    extends Expression {
  override def getKind: TokenType.TokenType = TokenType.assignmentExpression

  override def getChildren: List[Expression] =
    List(identifierToken, equalsToken, expression)
}

case class VariableDeclarationNode(keyword: Token,
                                   identifier: Token,
                                   equalsToken: Token,
                                   expression: Expression)
    extends Statement {
  override def getKind: TokenType = TokenType.variableDeclaration

  override def getChildren: List[Node] =
    List(keyword, identifier, equalsToken, expression)
}

case class FunctionCallNode(identifier: Token, expressions: List[Expression])
    extends Expression {
  override def getKind: TokenType = TokenType.funcCallExpression

  override def getChildren: List[Node] = expressions
}
