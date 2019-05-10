import TokenType.TokenType

abstract class Ast {
  def getKind: TokenType.TokenType

  def getChildren: List[Ast]
}

abstract class Expression extends Ast

abstract class Statement extends Ast

case class ExpressionStatement(expression: Expression) extends Statement {
  override def getKind: TokenType = TokenType.expressionStatement

  override def getChildren: List[Expression] = List(expression)
}

case class BlockStatement(openBraceToken: Tokens,
                          statements: List[Statement],
                          closeBraceToken: Tokens)
    extends Statement {
  override def getKind: TokenType = TokenType.blockStatement

  override def getChildren: List[Ast] = statements
}


case class FuncStatement(funcToken: Tokens,
                         identifier: Tokens,
                         parameters: List[ParamStatement],
                         returnType: ParamStatement,
                         body: Statement)
    extends Statement {
  override def getKind: TokenType = TokenType.funcStatement

  override def getChildren: List[Ast] =
    List(
      funcToken,
      identifier
    ) ++ parameters :+ returnType :+ body
}

case class ParamStatement(id: Tokens, paramType: Tokens) extends Statement {
  override def getKind: TokenType = TokenType.paramStatement

  override def getChildren: List[Ast] = List(
    id,
    paramType
  )
}

case class IfStatement(ifToken: Tokens,
                       condition: Expression,
                       expr1: Statement,
                       elseToken: Tokens,
                       expr2: Statement)
    extends Statement {
  override def getKind: TokenType = TokenType.ifStatement

  override def getChildren: List[Ast] =
    List(
      ifToken,
      condition,
      expr1,
      elseToken,
      expr2
    )
}

case class ForStatement(forToken: Tokens,
                        identifier: Tokens,
                        equalToken: Tokens,
                        low: Expression,
                        toToken: Tokens,
                        upper: Expression,
                        body: Statement)
    extends Statement {
  override def getKind: TokenType = TokenType.forStatement

  override def getChildren: List[Ast] =
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

case class WhileStatement(whileToken: Tokens,
                          condition: Expression,
                          body: Statement)
    extends Statement {
  override def getKind: TokenType = TokenType.whileStatement

  override def getChildren: List[Ast] =
    List(
      whileToken,
      condition,
      body
    )
}

case class SyntaxTree(diagnostics: DiagnosticsBag, root: CompilationUnit)

object SyntaxTree {
  def parse(text: String): SyntaxTree = {
    val parser = new Parser(Lexer.newLexer(text))
    parser.init()
    val expr = parser.parseCompilationUnit()
    SyntaxTree(parser.diagnostics, CompilationUnit(expr, TokenType.eof))
  }
}

case class CompilationUnit(statement: Statement, eof: TokenType)
    extends Statement {
  override def getKind: TokenType.TokenType = TokenType.compilationUnit

  override def getChildren: List[Ast] = List[Ast](statement)

  override def toString: String = s"ExpressionTree:$statement"
}

case class BinaryNode(left: Expression, op: Tokens, right: Expression)
    extends Expression {
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

case class BraceNode(left: Expression, op: Expression, right: Expression)
    extends Expression {
  override def getKind: TokenType.TokenType = TokenType.braceExpression

  override def getChildren: List[Expression] = List(left, op, right)

}

case class UnaryNode(op: Expression, operand: Expression) extends Expression {
  override def getKind: TokenType.TokenType = TokenType.unaryExpression

  override def getChildren: List[Expression] = List[Expression](op, operand)
}

case class NameNode(identifierToken: Tokens) extends Expression {
  override def getKind: TokenType.TokenType = TokenType.nameExpression

  override def getChildren: List[Expression] = List(identifierToken)
}

case class AssignmentNode(identifierToken: Tokens,
                          equalsToken: Tokens,
                          expression: Expression)
    extends Expression {
  override def getKind: TokenType.TokenType = TokenType.assignmentExpression

  override def getChildren: List[Expression] =
    List(identifierToken, equalsToken, expression)
}

case class VariableDeclarationNode(keyword: Tokens,
                                   identifier: Tokens,
                                   equalsToken: Tokens,
                                   expression: Expression)
    extends Statement {
  override def getKind: TokenType = TokenType.variableDeclaration

  override def getChildren: List[Ast] =
    List(keyword, identifier, equalsToken, expression)
}

case class FunctionCallNode(identifier: Tokens, expressions: List[Expression])
    extends Expression {
  override def getKind: TokenType = TokenType.funcCallExpression

  override def getChildren: List[Ast] = expressions
}
