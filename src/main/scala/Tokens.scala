import TokenType.TokenType

class Tokens(val tokenType: TokenType, val value: String, val span: Span)
    extends Expression {
  override def toString: String =
    s"<$tokenType :$value >  $span"

  override def getKind: TokenType = tokenType

  override def getChildren(): List[Ast] = null
}

object Tokens {
  def apply(tokenType: TokenType, value: String, span: Span): Tokens =
    new Tokens(tokenType, value, span)
}

object TokenType extends Enumeration {

  type TokenType = Value
  val
  //tokens
  keyword, func, identifier, literal, lb, rb, assign, equal, notequal, lt, gt,
  lte, gte, add, sub, mul, div, mod, and, or, not, pow, whiteSpace, newline,
  eof, wrong, openBraceToken, closeBraceToken, annotationToken, typeToken,
  //keyword
  falseKeyword, trueKeyword, varKeyword, letKeyword, ifKeyword, forKeyword,
  whileKeyword, elseKeyword, toKeyword, funcKeyword, returnKeyword,
  //expressions
  binaryExpression, numberExpression, unaryExpression, compilationUnit,
  braceExpression, nameExpression, assignmentExpression, funcCallExpression,
  //statement
  expressionStatement, variableDeclaration, blockStatement, ifStatement,
  forStatement, whileStatement, funcStatement, paramStatement = Value
}

object BindType extends Enumeration {
  type BindType = Value
  val identity, negation, addition, subtraction, multiplication, division, and,
  or, not, pow, mod, lt, lte, gt, gte, equal, notequal, //statement
  blockStatement, expressionStatement, variableDeclaration, ifStatement,
  whileStatement, forStatement, funcStatement = Value
}
