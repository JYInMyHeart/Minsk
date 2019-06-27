package parser

import parser.TokenType.TokenType
import sourceText.TextSpan

class Token(val tokenType: TokenType,
            val position:Int,
            val text:String,
            val value: Any)
    extends Expression {

  def span = TextSpan(position,text.length)

  override def getSpan: TextSpan = span

  override def toString: String =
    s"<$tokenType :$text >  $span"

  override def getKind: TokenType = tokenType

  def isMissing: Boolean = text == null


}

object Token {
  def apply(tokenType: TokenType,position:Int,text:String, value: Any): Token =
    new Token(tokenType, position,text,value)
}

object TokenType extends Enumeration {

  type TokenType = Value
  val
  //tokens
  keyword, func, identifierToken, literal,numberToken, openParenthesisToken, closeParenthesisToken,lmb,rmb,
  equalsToken, equalsEqualsToken,bangToken, bangEqualsToken, lessToken, greaterToken,colonToken,
  lessOrEqualsToken, greaterOrEqualsToken, plusToken, minusToken, starToken, slashToken,
  modToken, ampersandToken, ampersandAmpersandToken,pipeToken,pipePipeToken, tildeToken, hatToken, whiteSpaceToken, newline,
  eofToken, wrongToken, stringToken,openBraceToken, closeBraceToken, typeToken,commaToken,
  //keyword
  falseKeyword, trueKeyword, varKeyword, letKeyword, ifKeyword, forKeyword,
  whileKeyword, elseKeyword, toKeyword, funcKeyword, returnKeyword,
  //expressions
  binaryExpression, numberExpression, unaryExpression, compilationUnit, boolExpression,
  braceExpression, nameExpression, assignmentExpression, funcCallExpression,

  //statement
  expressionStatement, variableDeclaration, blockStatement, ifStatement,functionDeclaration,
  parameter,typeClause,globalStatement,
  forStatement, whileStatement, funcStatement, paramStatement = Value
}

object BindType extends Enumeration {
  type BindType = Value
  val identity, negation, addition, subtraction, multiplication, division, and,
  or, not, pow, mod, lt, lte, gt, gte, equal, notequal, //statement
  blockStatement, expressionStatement, variableDeclaration, ifStatement,
  literalExpression,assignmentExpression,errorExpression,conversionExpression,
  whileStatement, forStatement, funcStatement = Value
}
