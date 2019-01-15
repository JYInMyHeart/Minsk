import TokenType.TokenType
import TokenType._
import Facts._

class Parser(val lexer: Lexer) {
  private[this] var tokensList: List[Tokens] = List()
  private[this] var position: Int = _
  val diagnostics: DiagnosticsBag = DiagnosticsBag()
  diagnostics.concat(lexer.diagnostics)

  def init(): Unit = {
    var token = lexer.nextToken()
    while (token.tokenType != eof) {
      if (token.tokenType != wrong
        && token.tokenType != whiteSpace)
        tokensList :+= token
      token = lexer.nextToken()
    }
  }


  def peek(offset: Int): Tokens = {
    val size = tokensList.length
    val index: Int = position + offset
    if (index >= size) {
      if (tokensList.nonEmpty)
        return Tokens(TokenType.eof, "eof", tokensList.last.span)
      else
        return Tokens(TokenType.eof, "eof", Span(0, 0))
    }
    tokensList(index)
  }

  def current: Tokens = peek(0)

  private def nextToken: Tokens = {
    val currentToken = current
    position += 1
    currentToken
  }

  def eat(tokenType: TokenType): Tokens = {
    if (tokenType == current.tokenType)
      return nextToken
    diagnostics.reportUnexpectedToken(current.span, current.tokenType, tokenType)
    Tokens(tokenType, null, current.span)
  }

  def parseCompilationUnit(): Statement = {
    val statement = parseStatement()
    eat(TokenType.eof)
    CompilationUnit(statement, TokenType.eof)
  }


  def parseBlockStatement(): BlockStatement = {
    var statements: List[Statement] = List()
    val openBrace = eat(TokenType.openBraceToken)
    while (current.tokenType != eof
      && current.tokenType != closeBraceToken) {
      val statement = parseStatement()
      statements :+= statement
    }
    val closeBrace = eat(closeBraceToken)
    BlockStatement(openBrace, statements, closeBrace)
  }

  def parseExpressionStatement() =
    ExpressionStatement(parseExpression())


  def parseStatement(): Statement = {
    if (current.tokenType == TokenType.openBraceToken)
      return parseBlockStatement()
    parseExpressionStatement()
  }

  def parseExpression(): Expression = {
    parseAssignmentExpression()
  }

  def parseAssignmentExpression(): Expression = {
    if (peek(0).tokenType == TokenType.identifier &&
      peek(1).tokenType == TokenType.assign) {
      val identifierToken = nextToken
      val operatorToken = nextToken
      val right = parseAssignmentExpression()
      return AssignmentNode(identifierToken, operatorToken, right)
    }
    parseBinaryExpression()
  }


  def parseBinaryExpression(parentPrecedence: Int = -1): Expression = {
    var left: Expression = null
    val unaryOperatorPrecedence = getUnaryOperatorPrecedence(current.tokenType)
    if (unaryOperatorPrecedence != -1 && unaryOperatorPrecedence >= parentPrecedence) {
      val operatorToken = nextToken
      val operand = parseBinaryExpression(unaryOperatorPrecedence)
      return UnaryNode(operatorToken, operand)
    } else
      left = parsePrimaryExpression()
    var enable = true
    while (enable) {
      val precedence = getBinaryOperatorPrecedence(current.tokenType)
      if (precedence == -1 || precedence <= parentPrecedence)
        enable = false
      else {
        val operatorToken = nextToken
        val right = parseBinaryExpression(precedence)
        left = BinaryNode(left, operatorToken, right)
      }
    }
    left
  }


  def parsePrimaryExpression(): Expression = {
    current.tokenType match {
      case TokenType.lb =>
        val left = nextToken
        val expression = parseExpression()
        val right = eat(TokenType.rb)
        BraceNode(left, expression, right)
      case x if x == TokenType.trueKeyword || x == TokenType.falseKeyword =>
        val token = current
        nextToken
        LiteralNode(token)
      case TokenType.identifier =>
        val token = current
        nextToken
        NameNode(token)
      case _ =>
        val literalNode = eat(TokenType.literal)
        LiteralNode(literalNode)
    }
  }
}
