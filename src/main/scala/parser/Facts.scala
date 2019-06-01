package parser

import parser.TokenType.TokenType

object Facts {

  def getText(tokenType: TokenType) = {
    tokenType match {
      case TokenType.`openParenthesisToken`  => "("
      case TokenType.`closeParenthesisToken` => ")"
      case TokenType.`equalsToken`           => "="
      case TokenType.`equalsEqualsToken`     => "=="
      case TokenType.`bangEqualsToken`       => "!="
      case TokenType.`bangToken`             => "!"
      case TokenType.`lessToken`             => "<"
      case TokenType.`greaterToken`          => ">"
      case TokenType.`lessOrEqualsToken`     => "<="
      case TokenType.`greaterOrEqualsToken`  => ">="
      case TokenType.`plusToken`             => "+"
      case TokenType.`minusToken`            => "-"
      case TokenType.`starToken`             => "*"
      case TokenType.`slashToken`            => "/"
      case TokenType.mod                     => "%"
      case TokenType.ampersandToken          => "&"
      case TokenType.`pipeToken`             => "|"
      case TokenType.`tildeToken`            => "~"
      case TokenType.`hatToken`              => "^"
      case TokenType.`eofToken`              => "\0"
      case TokenType.annotationToken         => ":"
      case TokenType.openBraceToken          => "{"
      case TokenType.closeBraceToken         => "}"
      case TokenType.falseKeyword            => "false"
      case TokenType.trueKeyword             => "true"
      case TokenType.varKeyword              => "var"
      case TokenType.letKeyword              => "let"
      case TokenType.ifKeyword               => "if"
      case TokenType.elseKeyword             => "else"
      case TokenType.forKeyword              => "for"
      case TokenType.whileKeyword            => "while"
      case TokenType.toKeyword               => "to"
      case TokenType.funcKeyword             => "def"
      case TokenType.returnKeyword           => "return"
      case _                                 => null
    }
  }

  def getUnaryOperatorKinds: List[TokenType.Value] = {
    val tokenTypes = TokenType.values
    tokenTypes.filter(getUnaryOperatorPrecedence(_) > 0).toList
  }

  def getBinaryOperatorKinds: List[TokenType.Value] = {
    val tokenTypes = TokenType.values
    tokenTypes.filter(getBinaryOperatorPrecedence(_) > 0).toList
  }

  def getUnaryOperatorPrecedence(tokenType: TokenType): Int =
    tokenType match {
      case x
          if x == TokenType.plusToken
            | x == TokenType.minusToken =>
        6
      case TokenType.`tildeToken` =>
        6
      case _ =>
        -1
    }

  def getBinaryOperatorPrecedence(tokenType: TokenType): Int = {
    tokenType match {
      case x
          if x == TokenType.plusToken
            | x == TokenType.minusToken =>
        4
      case x
          if x == TokenType.slashToken
            | x == TokenType.mod
            | x == TokenType.starToken
            | x == TokenType.hatToken =>
        5
      case x
          if x == TokenType.ampersandToken
            | x == TokenType.pipeToken =>
        1
      case x
          if x == TokenType.lessToken
            | x == TokenType.lessOrEqualsToken
            | x == TokenType.greaterToken
            | x == TokenType.greaterOrEqualsToken
            | x == TokenType.equalsEqualsToken
            | x == TokenType.bangEqualsToken =>
        3
      case _ =>
        -1
    }
  }

  def getKeywordType(text: String): TokenType.Value = text match {
    case "else"    => TokenType.elseKeyword
    case "false"   => TokenType.falseKeyword
    case "for"     => TokenType.forKeyword
    case "if"      => TokenType.ifKeyword
    case "let"     => TokenType.letKeyword
    case "to"      => TokenType.toKeyword
    case "true"    => TokenType.trueKeyword
    case "var"     => TokenType.varKeyword
    case "while"   => TokenType.whileKeyword
    case "def"     => TokenType.funcKeyword
    case "return"  => TokenType.returnKeyword
    case "Integer" => TokenType.typeToken
    case "Double"  => TokenType.typeToken
    case _         => TokenType.identifierToken
  }

}
