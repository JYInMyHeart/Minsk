package parser

import eval.DiagnosticsBag
import parser.TokenType.TokenType
import sourceText.{SourceText, TextSpan}
import symbol.TypeSymbol

class Lexer(val sourceText: SourceText) {
  private[this] var position: Int = _
  private[this] var start: Int = _
  private[this] var kind: TokenType = _
  private[this] var value: Any = _
  var diagnostics: DiagnosticsBag = DiagnosticsBag()

  private[this] def current = peek(0)
  private[this] def lookahead = peek(1)

  def peek(offset: Int): Char = {
    val index = position + offset
    if (index >= sourceText.length)
      return '\0'
    val ch = sourceText.at(index)
    ch
  }

  def readString(): Unit = {
    position += 1
    val sb: StringBuilder = new StringBuilder()
    var done = false
    while (!done) {
      current match {
        case x if x == '\0' | x == '\r' | x == '\n' =>
          val span = TextSpan(start, 1)
          diagnostics.reportUnterminatedString(span)
          done = true
        case '"' =>
          if (lookahead == '"') {
            sb += current
            position += 2
          } else {
            position += 1
            done = true
          }
        case _ =>
          sb += current
          position += 1
      }
    }
    kind = TokenType.stringToken
    value = sb.toString()
  }

  def readNumber(): Unit = {
    while (Character.isDigit(current)) position += 1
    val length = position - start
    val text = sourceText.toString(start, start + length)
    try {
      text.toInt
    } catch {
      case _: Exception =>
        diagnostics.reportInvalidNumber(TextSpan(start, length),
                                        text,
                                        TypeSymbol.Int)
    }
    value = text.toInt
    kind = TokenType.numberToken
  }

  def readWhiteSpace(): Unit = {
    while (Character.isWhitespace(current)) position += 1
    kind = TokenType.whiteSpaceToken
  }

  def readIdentifierOrKeyword(): Unit = {
    while (Character.isLetter(current)) {
      position += 1
    }
    val length = position - start
    val text = sourceText.toString(start, start + length)
    kind = Facts.getKeywordType(text)
  }

  def lex(): Token = {
    start = position
    kind = TokenType.wrongToken
    value = null

    current match {
      case '\0' =>
        kind = TokenType.eofToken
      case '+' =>
        kind = TokenType.plusToken
        position += 1
      case '-' =>
        kind = TokenType.minusToken
        position += 1
      case '*' =>
        kind = TokenType.starToken
        position += 1
      case '/' =>
        kind = TokenType.slashToken
        position += 1
      case '(' =>
        kind = TokenType.openParenthesisToken
        position += 1
      case ')' =>
        kind = TokenType.closeParenthesisToken
        position += 1
      case '{' =>
        kind = TokenType.openBraceToken
        position += 1
      case '}' =>
        kind = TokenType.closeBraceToken
        position += 1
      case ',' =>
        kind = TokenType.commaToken
        position += 1
      case '~' =>
        kind = TokenType.tildeToken
        position += 1
      case '^' =>
        kind = TokenType.hatToken
        position += 1
      case '%' =>
        kind = TokenType.modToken
        position += 1
      case '&' =>
        position += 1
        if (current != '&') {
          kind = TokenType.ampersandToken
        } else {
          kind = TokenType.ampersandAmpersandToken
          position += 1
        }
      case '|' =>
        position += 1
        if (current != '|') {
          kind = TokenType.pipeToken
        } else {
          kind = TokenType.pipePipeToken
          position += 1
        }
      case '=' =>
        position += 1
        if (current != '=') {
          kind = TokenType.equalsToken
        } else {
          kind = TokenType.equalsEqualsToken
          position += 1
        }
      case '!' =>
        position += 1
        if (current != '=') {
          kind = TokenType.bangToken
        } else {
          kind = TokenType.bangEqualsToken
          position += 1
        }
      case '<' =>
        position += 1
        if (current != '=') {
          kind = TokenType.lessToken
        } else {
          kind = TokenType.lessOrEqualsToken
          position += 1
        }
      case '>' =>
        position += 1
        if (current != '=') {
          kind = TokenType.greaterToken
        } else {
          kind = TokenType.greaterOrEqualsToken
          position += 1
        }
      case '"' =>
        readString()
      case x if Character.isDigit(x) =>
        readNumber()
      case x
          if x == ' '
            | x == '\t'
            | x == '\n'
            | x == '\r' =>
        readWhiteSpace()
      case _ =>
        if (Character.isLetter(current)){
          readIdentifierOrKeyword()
        }
        else if (Character.isWhitespace(current))
          readWhiteSpace()
        else {
          diagnostics.reportBadCharacter(position, current)
          position += 1
        }
    }
    val length = position - start
    var text = Facts.getText(kind)
    if (text == null)
      text = sourceText.toString(start, start + length)
    Token(kind, position, text, value)
  }
}

object Lexer {
  def apply(sourceText: SourceText): Lexer = new Lexer(sourceText)
}
