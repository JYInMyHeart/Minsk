import TokenType.TokenType

abstract class Ast {
  def getKind: TokenType.TokenType

  def getChildren: List[Expression]
}

abstract class Expression extends Ast

case class SyntaxTree(diagnostics: DiagnosticsBag,
                      root: CompilationUnit)

object SyntaxTree{
  def parse(text: String): SyntaxTree = {
    val parser = new Parser(Lexer.newLexer(text))
    parser.init()
    val expr = parser.parseTreeExpression()
    SyntaxTree(parser.diagnostics,CompilationUnit(expr,TokenType.eof))
  }
}

case class CompilationUnit( expr: Expression,
                              eof:TokenType) extends Expression {
  override def getKind: TokenType.TokenType = TokenType.compilationUnit

  override def getChildren: List[Expression] = List[Expression](expr)

  override def toString: String = s"ExpressionTree:$expr"
}

case class BinaryNode(left: Expression,
                      op: Tokens,
                      right: Expression) extends Expression {
  override def getKind: TokenType.TokenType = TokenType.binaryExpression

  override def getChildren: List[Expression] = {
    List[Expression](left, op, right)
  }

  override def toString: String = s"BinaryNode:${left.getKind}"
}

case class LiteralNode(value: Tokens) extends Expression {
  override def getKind: TokenType.TokenType = TokenType.numberExpression

  override def getChildren: List[Expression] = {
    List[Expression](value)
  }
}

case class BraceNode(left: Expression,
                     op: Expression,
                     right: Expression) extends Expression {
  override def getKind: TokenType.TokenType = TokenType.braceExpression

  override def getChildren: List[Expression] = List(left, op, right)

}

case class UnaryNode(op: Expression,
                     operand: Expression) extends Expression {
  override def getKind: TokenType.TokenType = TokenType.unaryExpression

  override def getChildren: List[Expression] = List[Expression](op, operand)
}

case class NameNode(identifierToken: Tokens) extends Expression {
  override def getKind: TokenType.TokenType = TokenType.nameExpression

  override def getChildren: List[Expression] = List(identifierToken)
}

case class AssignmentNode(identifierToken: Tokens,
                          equalsToken: Tokens,
                          expression: Expression) extends Expression {
  override def getKind: TokenType.TokenType = TokenType.assignmentExpression

  override def getChildren: List[Expression] = List(identifierToken, equalsToken, expression)
}
