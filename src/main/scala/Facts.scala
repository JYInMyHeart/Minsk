import TokenType.TokenType

object Facts {

  def getText(tokenType: TokenType) = {
    tokenType match {
      case TokenType.lb              => "("
      case TokenType.rb              => ")"
      case TokenType.assign          => "="
      case TokenType.equal           => "=="
      case TokenType.notequal        => "!="
      case TokenType.lt              => "<"
      case TokenType.gt              => ">"
      case TokenType.lte             => "<="
      case TokenType.gte             => ">="
      case TokenType.add             => "+"
      case TokenType.sub             => "-"
      case TokenType.`mul`            => "*"
      case TokenType.div             => "/"
      case TokenType.mod             => "%"
      case TokenType.and             => "&"
      case TokenType.or              => "|"
      case TokenType.not             => "!"
      case TokenType.pow             => "^"
      case TokenType.eof             => "\0"
      case TokenType.annotationToken => ":"
      case TokenType.openBraceToken  => "{"
      case TokenType.closeBraceToken => "}"
      case TokenType.falseKeyword    => "false"
      case TokenType.trueKeyword     => "true"
      case TokenType.varKeyword      => "var"
      case TokenType.letKeyword      => "let"
      case TokenType.ifKeyword       => "if"
      case TokenType.elseKeyword     => "else"
      case TokenType.forKeyword      => "for"
      case TokenType.whileKeyword    => "while"
      case TokenType.toKeyword       => "to"
      case TokenType.funcKeyword     => "def"
      case TokenType.returnKeyword   => "return"
      case _                         => null
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
          if x == TokenType.add
            | x == TokenType.sub =>
        6
      case TokenType.not =>
        6
      case _ =>
        -1
    }

  def getBinaryOperatorPrecedence(tokenType: TokenType): Int = {
    tokenType match {
      case x
          if x == TokenType.add
            | x == TokenType.sub =>
        4
      case x
          if x == TokenType.div
            | x == TokenType.mod
            | x == TokenType.mul
            | x == TokenType.pow =>
        5
      case x
          if x == TokenType.and
            | x == TokenType.or =>
        1
      case x
          if x == TokenType.lt
            | x == TokenType.lte
            | x == TokenType.gt
            | x == TokenType.gte
            | x == TokenType.equal
            | x == TokenType.notequal =>
        3
      case _ =>
        -1
    }
  }

  def getKeywordType(text: String): TokenType.Value = {
    text match {
      case "true"  => TokenType.trueKeyword
      case "false" => TokenType.falseKeyword
      case _       => TokenType.identifier
    }
  }

}
