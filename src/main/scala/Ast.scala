import TokenType.TokenType

abstract class Ast {
  def getKind(): TokenType.TokenType

  def getChildren(): List[Expression]
}

abstract class Expression extends Ast

case class SyntaxTree(diagnostics: DiagnosticsBag,
                      root: Expression,
                      eofToken: TokenType.TokenType)

object SyntaxTree{
  def parse(text: String): SyntaxTree = {
    val parser = new Parser(Lexer.newLexer(text))
    parser.init()
    val expr = parser.parseTreeExpression()
    SyntaxTree(parser.diagnostics,expr,TokenType.eof)
  }
}

sealed class CompilationUnit(val expr: Expression,
                             val eof:TokenType) extends Expression {
  override def getKind(): TokenType.TokenType = TokenType.compilationUnit

  override def getChildren(): List[Expression] = List[Expression](expr)

  override def toString: String = s"ExpressionTree:$expr"
}

sealed class BinaryNode(val left: Expression,
                        val op: Tokens,
                        val right: Expression) extends Expression {
  override def getKind(): TokenType.TokenType = TokenType.binaryExpression

  override def getChildren(): List[Expression] = {
    List[Expression](left, op, right)
  }

  override def toString: String = s"BinaryNode:${left.getKind()}"
}

sealed class LiteralNode(val value: Tokens) extends Expression {
  override def getKind(): TokenType.TokenType = TokenType.numberExpression

  override def getChildren(): List[Expression] = {
    List[Expression](value)
  }
}

sealed class BraceNode(val left: Expression,
                       val op: Expression,
                       val right: Expression) extends Expression {
  override def getKind(): TokenType.TokenType = TokenType.braceExpression

  override def getChildren(): List[Expression] = List(left, op, right)

}

sealed class UnaryNode(val op: Expression,
                       val oprand: Expression) extends Expression {
  override def getKind(): TokenType.TokenType = TokenType.unaryExpression

  override def getChildren(): List[Expression] = List[Expression](op, oprand)
}

sealed class NameNode(val identifierToken: Tokens) extends Expression {
  override def getKind(): TokenType.TokenType = TokenType.nameExpression

  override def getChildren(): List[Expression] = List(identifierToken)
}

sealed class AssignmentNode(val identifierToken: Tokens,
                            val equalsToken: Tokens,
                            val expression: Expression) extends Expression {
  override def getKind(): TokenType.TokenType = TokenType.assignmentExpression

  override def getChildren(): List[Expression] = List(identifierToken, equalsToken, expression)
}
