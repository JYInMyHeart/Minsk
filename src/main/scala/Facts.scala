import TokenType.TokenType

object Facts {


  def getText(tokenType: TokenType) = {
    tokenType match {
      case TokenType.lb => "("
      case TokenType.rb => ")"
      case TokenType.assign => "="
      case TokenType.equal => "=="
      case TokenType.notequal => "!="
      case TokenType.lt => "<"
      case TokenType.gt => ">"
      case TokenType.lte => "<="
      case TokenType.gte => ">="
      case TokenType.add => "+"
      case TokenType.sub => "-"
      case TokenType.plus => "*"
      case TokenType.div => "/"
      case TokenType.mod => "%"
      case TokenType.and => "&"
      case TokenType.or => "|"
      case TokenType.not => "!"
      case TokenType.pow => "^"
      case TokenType.eof => "\0"
      case TokenType.falseKeyword => "false"
      case TokenType.trueKeyword => "true"
      case _ => null
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
      case x if x == TokenType.add
        | x == TokenType.sub =>
        3
      case TokenType.not =>
        1
      case _ =>
        -1
    }


  def getBinaryOperatorPrecedence(tokenType: TokenType): Int = {
    tokenType match {
      case x if x == TokenType.add
        | x == TokenType.sub =>
        1
      case x if x == TokenType.div
        | x == TokenType.mod
        | x == TokenType.plus
        | x == TokenType.pow =>
        2
      case x if x == TokenType.and
        | x == TokenType.or =>
        1
      case x if x == TokenType.lt
        | x == TokenType.lte
        | x == TokenType.gt
        | x == TokenType.gte
        | x == TokenType.equal
        | x == TokenType.notequal =>
        0
      case TokenType.assign =>
        6
      case _ =>
        -1
    }
  }


  def getKeywordType(text:String): TokenType.Value = {
    text match {
      case "true" => TokenType.trueKeyword
      case "false" => TokenType.falseKeyword
      case _ => TokenType.identifier
    }
  }



}
