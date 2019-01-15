
import java.io.{ByteArrayInputStream, PushbackInputStream}

import TokenType._

class Lexer(val pushBackInputStream: PushbackInputStream) {
  var ch: Char = _
  var lineCount: Int = 1
  var columnCount: Int = _
  var diagnostics: DiagnosticsBag = DiagnosticsBag()

  def read(): Unit = {
    val c = pushBackInputStream.read()
    ch = c.toChar
    columnCount += 1
  }

  def unRead(c: Char): Unit = {
    pushBackInputStream.unread(c.toInt)
  }

  def unRead(s: String): Unit = {
    for (i <- s.length - 1 to 0 by -1)
      pushBackInputStream.unread(s.charAt(i).toInt)
  }

  def getKeyWordType(text: String): TokenType.Value = {
    text match {
      case "false" => falseKeyword
      case "true" => trueKeyword
      case "var" => varKeyword
      case "let" => letKeyword
      case "if" => ifKeyword
      case "else" => elseKeyword
      case "while" => whileKeyword
      case "for" => forKeyword
      case _ => identifier
    }
  }

  def nextToken(): Tokens = {
    read()
    ch match {
      case x if x == ' ' =>
        Tokens(whiteSpace, null, Span(lineCount, columnCount))
      case x if x == '\n' | x == '\r' =>
        lineCount += 1
        columnCount = 0
        Tokens(newline, null, Span(lineCount, columnCount))
      case x if Character.isDigit(x) =>
        Tokens(literal, getNum, Span(lineCount, columnCount))
      case x if x == '_' || Character.isLetter(x) =>
        val text = getChars
        val tokenType = getKeyWordType(text)
        Tokens(tokenType, text, Span(lineCount, columnCount))
      case '+' => Tokens(add, "+", Span(lineCount, columnCount))
      case '*' => Tokens(plus, "*", Span(lineCount, columnCount))
      case '/' => Tokens(div, "/", Span(lineCount, columnCount))
      case '%' => Tokens(mod, "%", Span(lineCount, columnCount))
      case '^' => Tokens(pow, "^", Span(lineCount, columnCount))
      case '&' => Tokens(and, "&", Span(lineCount, columnCount))
      case '|' => Tokens(or, "|", Span(lineCount, columnCount))
      case '!' =>
        read()
        ch match {
          case '=' =>
            Tokens(notequal, "!=", Span(lineCount, columnCount))
          case _ =>
            unRead(ch)
            Tokens(not, "!", Span(lineCount, columnCount))
        }
      case x if x == '-' =>
        read()
        var token: Tokens = null
        ch match {
          case '>' =>
            token = Tokens(keyword, "->", Span(lineCount, columnCount))
          case _ =>
            unRead(ch)
            token = Tokens(sub, "-", Span(lineCount, columnCount))
        }
        token
      case x if x == '\"' =>
        Tokens(literal, getStr, Span(lineCount, columnCount))
      case '(' =>
        Tokens(lb, "(", Span(lineCount, columnCount))
      case ')' =>
        Tokens(rb, ")", Span(lineCount, columnCount))
      case '{' =>
        Tokens(openBraceToken, "(", Span(lineCount, columnCount))
      case '}' =>
        Tokens(closeBraceToken, "(", Span(lineCount, columnCount))
      case '<' =>
        read()
        ch match {
          case '=' =>
            Tokens(lte, "<=", Span(lineCount, columnCount))
          case _ =>
            unRead(ch)
            Tokens(lt, "<", Span(lineCount, columnCount))
        }
      case '>' =>
        read()
        ch match {
          case '=' =>
            Tokens(gte, ">=", Span(lineCount, columnCount))
          case _ =>
            unRead(ch)
            Tokens(gt, ">", Span(lineCount, columnCount))
        }
      case '=' =>
        read()
        ch match {
          case '=' =>
            Tokens(equal, "==", Span(lineCount, columnCount))
          case _ =>
            unRead(ch)
            Tokens(assign, "=", Span(lineCount, columnCount))
        }
      case '\0' =>
        Tokens(eof, "EOF", Span(lineCount, columnCount))
      case _ =>
        diagnostics.report(
          Span(lineCount, columnCount),
          s"error:bad character input $ch at line $lineCount ,$columnCount"
        )
        Tokens(wrong, "wrong", Span(lineCount, columnCount))
    }

  }


  def satisfied(c: Char): Boolean = ch == c

  def string(str: String): Boolean = {
    for (i <- str.indices) {
      if (satisfied(str.charAt(i)))
        read()
      else {
        unRead(str.substring(0, i))
        return false
      }
    }
    true
  }

  def getNum: String = {
    var str: String = ""
    while (Character.isDigit(ch)) {
      str += ch.toString
      read()
    }
    unRead(ch)
    str
  }

  def getStr: String = {
    var str: String = ""
    read()
    while (ch != '\"') {
      str += ch.toString
      read()
    }
    str
  }

  def getChars: String = {
    var str: String = ""
    while (Character.isLetter(ch) || Character.isDigit(ch) || ch == '_') {
      str += ch.toString
      read()
    }
    unRead(ch)
    str
  }
}

object Lexer {
  def newLexer(expr: String): Lexer = {
    new Lexer(new PushbackInputStream(new ByteArrayInputStream((expr + '\0').getBytes), 5))
  }

  def apply(pushbackInputStream: PushbackInputStream): Lexer = new Lexer(
    pushbackInputStream
  )
}
