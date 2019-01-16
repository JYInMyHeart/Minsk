import Facts._
import TokenType.{TokenType, _}

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


  def parseVariableDeclaration(): VariableDeclarationNode = {
    val expected =
      if (current.tokenType == letKeyword)
        letKeyword
      else
        varKeyword
    val keyword = eat(expected)
    val id = eat(identifier)
    val eq = eat(assign)
    val initializer = parseExpression()
    VariableDeclarationNode(keyword, id, eq, initializer)
  }

  def parseIfStatement(): IfStatement = {
    val ifKeyword = eat(TokenType.ifKeyword)
    eat(TokenType.lb)
    val condition = parseExpression()
    eat(TokenType.rb)
    val expr1 = parseStatement()
    var elseKeyword: Tokens = null
    var expr2: Statement = null
    if (current.tokenType == TokenType.elseKeyword) {
      elseKeyword = eat(TokenType.elseKeyword)
      expr2 = parseStatement()
    }
    IfStatement(ifKeyword, condition, expr1, elseKeyword, expr2)
  }

  def parseStatement(): Statement = {
    current.tokenType match {
      case TokenType.openBraceToken =>
        parseBlockStatement()
      case x if x == TokenType.letKeyword
        | x == varKeyword =>
        parseVariableDeclaration()
      case TokenType.ifKeyword =>
        parseIfStatement()
      case _ =>
        parseExpressionStatement()
    }
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
      left = UnaryNode(operatorToken, operand)
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
        parseParenthesizedExpression()
      case x if x == TokenType.trueKeyword || x == TokenType.falseKeyword =>
        parseBooleanLiteral()
      case TokenType.identifier =>
        parseNameExpression()
      case TokenType.literal =>
        parseNumberLiteral()
      case _ =>
        parseNameExpression()
    }
  }

  private def parseBooleanLiteral(): LiteralNode = {
    val token = current
    nextToken
    LiteralNode(token)
  }

  private def parseParenthesizedExpression(): BraceNode = {
    val left = nextToken
    val expression = parseExpression()
    val right = eat(TokenType.rb)
    BraceNode(left, expression, right)
  }

  private def parseNameExpression(): NameNode = {
    val id = eat(identifier)
    NameNode(id)
  }

  private def parseNumberLiteral(): LiteralNode = {
    val number = eat(literal)
    LiteralNode(number)
  }
}
