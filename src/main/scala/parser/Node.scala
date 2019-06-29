package parser

import eval.DiagnosticsBag
import parser.TokenType.TokenType
import sourceText.{SourceText, TextSpan}
import symbol.TypeSymbol

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
      property.setAccessible(true)
      if (classOf[Node].isAssignableFrom(property.getType)) {
        val child = property.get(this).asInstanceOf[Node]
        if (child != null)
          result += child
      } else if (classOf[List[Node]].isAssignableFrom(property.getType)) {
        val child = property.get(this).asInstanceOf[List[Node]]
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

}

case class BlockStatement(openBraceToken: Token,
                          statements: List[Statement],
                          closeBraceToken: Token)
    extends Statement {
  override def getKind: TokenType = TokenType.blockStatement

}

case class FuncStatement(funcToken: Token,
                         identifier: Token,
                         parameters: List[ParamStatement],
                         returnType: ParamStatement,
                         body: Statement)
    extends Statement {
  override def getKind: TokenType = TokenType.funcStatement

}

case class ParamStatement(id: Token, paramType: Token) extends Statement {
  override def getKind: TokenType = TokenType.paramStatement

}

case class IfStatement(ifToken: Token,
                       condition: Expression,
                       expr1: Statement,
                       elseToken: Token,
                       expr2: Statement)
    extends Statement {
  override def getKind: TokenType = TokenType.ifStatement

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

}

case class WhileStatement(whileToken: Token,
                          condition: Expression,
                          body: Statement)
    extends Statement {
  override def getKind: TokenType = TokenType.whileStatement

}

case class GlobalStatementNode(statement:Statement) extends Member{
  override def getKind: TokenType = TokenType.globalStatement
}

case class SyntaxTree(diagnostics: DiagnosticsBag, root: CompilationUnit)

object SyntaxTree {
  def parse(text: String): SyntaxTree = {
    val sourceText = SourceText(text)
    val parser = Parser(sourceText)
    val expr = parser.parseCompilationUnit()
    SyntaxTree(parser.diagnostics, expr)
  }
}

case class CompilationUnit(members: List[Member], eof: Token)
    extends Statement {
  override def getKind: TokenType.TokenType = TokenType.compilationUnit

  override def toString: String = s"ExpressionTree:$members"
}
abstract class Member() extends Node


case class BinaryNode(left: Expression, op: Token, right: Expression)
    extends Expression {
  override def getKind: TokenType.TokenType = TokenType.binaryExpression



  override def toString: String = s"parser.BinaryNode:${left.getKind}"
}

case class LiteralNode(value: Token) extends Expression {
  override def getKind: TokenType.TokenType = TokenType.numberExpression
}

case class BraceNode(left: Expression, op: Expression, right: Expression)
    extends Expression {
  override def getKind: TokenType.TokenType = TokenType.braceExpression


}

case class UnaryNode(op: Expression, operand: Expression) extends Expression {
  override def getKind: TokenType.TokenType = TokenType.unaryExpression

}

case class NameNode(identifierToken: Token) extends Expression {
  override def getKind: TokenType.TokenType = TokenType.nameExpression


}

case class AssignmentNode(identifierToken: Token,
                          equalsToken: Token,
                          expression: Expression)
    extends Expression {
  override def getKind: TokenType.TokenType = TokenType.assignmentExpression


}

case class VariableDeclarationNode(keyword: Token,
                                   identifier: Token,
                                   equalsToken: Token,
                                   expression: Expression)
    extends Statement {
  override def getKind: TokenType = TokenType.variableDeclaration
}

case class FunctionDeclarationNode(functionKeyword:Token,
                                   identifier:Token,
                                   openParenthesisToken:Token,
                                   parameters:List[ParameterNode],
                                   closeParenthesisToken:Token,
                                   functionType:TypeClauseNode,
                                   body:Statement) extends Member {
  override def getKind: TokenType = TokenType.functionDeclaration
}
case class ParameterNode(identifier:Token,
                         parameterType:TypeClauseNode) extends Node{
  override def getKind: TokenType = TokenType.parameter
}

case class TypeClauseNode(colonToken:Token,
                          identifier:Token) extends Node{
  override def getKind: TokenType = TokenType.typeClause
}


case class FunctionCallNode(identifier: Token, arguments: List[Expression])
    extends Expression {
  override def getKind: TokenType = TokenType.funcCallExpression


}
