package parser

import eval.DiagnosticsBag
import parser.TokenType.{
  TokenType,
  annotationToken,
  closeBraceToken,
  closeParenthesisToken,
  equalsToken,
  funcCallExpression,
  funcKeyword,
  identifierToken,
  letKeyword,
  literal,
  openParenthesisToken,
  returnKeyword,
  toKeyword,
  varKeyword,
  _
}
import sourceText.SourceText

import scala.collection.mutable.ListBuffer

class Parser(sourceText: SourceText) {
  private[this] var tokensList: List[Token] = _
  private[this] var position: Int = _
  val diagnostics: DiagnosticsBag = DiagnosticsBag()

  def init(): Unit = {
    val tokens = ListBuffer[Token]()
    val lexer = Lexer(sourceText)
    var token: Token = null
    do {
      token = lexer.lex()
      if (token.getKind != whiteSpaceToken
          && token.getKind != wrongToken) {
        tokens += token
      }
    } while (token.getKind != eofToken)
    tokensList = tokens.toList
    diagnostics.concat(lexer.diagnostics)
  }

  def peek(offset: Int): Token = {
    val index = offset + position
    if (index >= tokensList.size) {
      return tokensList.last
    }
    tokensList(index)
  }

  def current: Token = peek(0)

  private def nextToken: Token = {
    val currentToken = current
    position += 1
    currentToken
  }

  def eat(kind: TokenType): Token = {
    if (kind == current.getKind)
      return nextToken
    diagnostics.reportUnexpectedToken(current.span, current.getKind, kind)
    Token(kind, current.position, null, null)
  }

  def parseCompilationUnit(): Statement = {
    val statement = parseStatement()
    CompilationUnit(statement, TokenType.eofToken)
  }

  def parseBlockStatement(): BlockStatement = {
    var statements: List[Statement] = List()
    val openBrace = eat(TokenType.openBraceToken)
    while (current.tokenType != eofToken
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
    val id = eat(identifierToken)
    val eq = eat(equalsToken)
    val initializer = parseExpression()
    VariableDeclarationNode(keyword, id, eq, initializer)
  }

  def parseWhileStatement(): WhileStatement = {
    val whileKeyword = eat(TokenType.whileKeyword)
    eat(TokenType.openParenthesisToken)
    val condition = parseExpression()
    eat(TokenType.closeParenthesisToken)
    val body = parseStatement()
    WhileStatement(whileKeyword, condition, body)
  }

  def parseIfStatement(): IfStatement = {
    val ifKeyword = eat(TokenType.ifKeyword)
    eat(TokenType.openParenthesisToken)
    val condition = parseExpression()
    eat(TokenType.closeParenthesisToken)
    val expr1 = parseStatement()
    var elseKeyword: Token = null
    var expr2: Statement = null
    if (current.tokenType == TokenType.elseKeyword) {
      elseKeyword = eat(TokenType.elseKeyword)
      expr2 = parseStatement()
    }
    IfStatement(ifKeyword, condition, expr1, elseKeyword, expr2)
  }

  def parseForStatement(): ForStatement = {
    val forKeyword = eat(TokenType.forKeyword)
    val id = eat(identifierToken)
    val eq = eat(equalsToken)
    val initializer = parseExpression()
    val to = eat(toKeyword)
    val upper = parseExpression()
    val body = parseStatement()
    ForStatement(forKeyword, id, eq, initializer, to, upper, body)
  }

  def parseParamStatement(): ParamStatement = {
    val paramName = eat(identifierToken)
    eat(annotationToken)
    val paramType = eat(TokenType.typeToken)
    ParamStatement(paramName, paramType)
  }

  def parseFuncStatement(): FuncStatement = {
    val func = eat(funcKeyword)
    val id = eat(funcCallExpression)
    eat(openParenthesisToken)
    var parameters: List[ParamStatement] = List()
    while (current.tokenType != closeParenthesisToken) {
      parameters :+= parseParamStatement()
    }
    eat(closeParenthesisToken)
    eat(annotationToken)
    val paramType = eat(TokenType.typeToken)
    val returnType =
      ParamStatement(Token(returnKeyword, current.position,current.text,current.value), paramType)
    eat(equalsToken)
    val body = parseStatement()
    FuncStatement(func, id, parameters, returnType, body)
  }

//  def parseArrayStatement():ArrayStatement = {
//
//  }

  def parseStatement(): Statement = {
    current.tokenType match {
      case TokenType.openBraceToken =>
        parseBlockStatement()
      case x
          if x == TokenType.letKeyword
            | x == varKeyword =>
        parseVariableDeclaration()
      case TokenType.ifKeyword =>
        parseIfStatement()
      case TokenType.whileKeyword =>
        parseWhileStatement()
      case TokenType.forKeyword =>
        parseForStatement()
      case TokenType.funcKeyword =>
        parseFuncStatement()
      case _ =>
        parseExpressionStatement()
    }
  }

  def parseExpression(): Expression = {
    parseAssignmentExpression()
  }

  def parseAssignmentExpression(): Expression = {
    if (peek(0).tokenType == TokenType.identifierToken &&
        peek(1).tokenType == TokenType.equalsToken) {
      val identifierToken = nextToken
      val operatorToken = nextToken
      val right = parseAssignmentExpression()
      return AssignmentNode(identifierToken, operatorToken, right)
    }
    parseBinaryExpression()
  }

  def parseBinaryExpression(parentPrecedence: Int = -1): Expression = {
    var left: Expression = null
    val unaryOperatorPrecedence = Facts.getUnaryOperatorPrecedence(current.tokenType)
    if (unaryOperatorPrecedence != -1 && unaryOperatorPrecedence >= parentPrecedence) {
      val operatorToken = nextToken
      val operand = parseBinaryExpression(unaryOperatorPrecedence)
      left = UnaryNode(operatorToken, operand)
    } else
      left = parsePrimaryExpression()
    var enable = true
    while (enable) {
      val precedence = Facts.getBinaryOperatorPrecedence(current.tokenType)
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
      case TokenType.`openParenthesisToken` =>
        parseParenthesizedExpression()
      case x if x == TokenType.trueKeyword || x == TokenType.falseKeyword =>
        parseBooleanLiteral()
      case TokenType.`identifierToken` =>
        parseNameExpression()
      case TokenType.numberToken =>
        parseNumberLiteral()
      case TokenType.funcCallExpression =>
        parseFuncCallExpression()
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
    val right = eat(TokenType.closeParenthesisToken)
    BraceNode(left, expression, right)
  }

  private def parseNameExpression(): NameNode = {
    val id = eat(identifierToken)
    NameNode(id)
  }

  private def parseNumberLiteral(): LiteralNode = {
    val number = eat(numberToken)
    LiteralNode(number)
  }

  private def parseFuncCallExpression(): FunctionCallNode = {
    val id = eat(funcCallExpression)
    eat(openParenthesisToken)
    var paramsList = List[Expression]()
    while (current.tokenType != closeParenthesisToken) {
      val expression = parseExpression()
      paramsList :+= expression
    }
    eat(closeParenthesisToken)
    FunctionCallNode(id, paramsList)
  }
}

object Parser{
  def apply(sourceText: SourceText): Parser = new Parser(sourceText)
}
