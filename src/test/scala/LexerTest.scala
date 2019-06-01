import parser.TokenType.TokenType
import parser.{Facts, TokenType, Parser}
import sourceText.SourceText

import scala.util.Random

class LexerTest extends UnitSpec {
  val dynamicTokens: List[(TokenType, String)] = List(
    //    (parser.TokenType.keyword,""),
    //      (parser.TokenType.func,""),
    (TokenType.identifierToken, "a_12"),
    (TokenType.literal, "1234"),
    (TokenType.eofToken, "\0"),
    (TokenType.falseKeyword, "false"),
    (TokenType.trueKeyword, "true"),
    (TokenType.ifKeyword, "if"),
    (TokenType.elseKeyword, "else"),
    (TokenType.forKeyword, "for"),
    (TokenType.whileKeyword, "while"),
    (TokenType.varKeyword, "var"),
    (TokenType.letKeyword, "let"),
    (TokenType.toKeyword, "to"),
    (TokenType.funcKeyword, "def"),
    (TokenType.returnKeyword, "return")
  )

  val fixedTokens: List[(TokenType.Value, String)] =
    TokenType.values
      .map(k => (k, Facts.getText(k)))
      .filter(_._2 != null)
      .toList

  val tokens: List[(TokenType.Value, String)] = fixedTokens

  val separators = List(
    (TokenType.whiteSpaceToken, " "),
    (TokenType.whiteSpaceToken, "\r"),
  )

  val others = List(
    (TokenType.newline, "\n"),
    (TokenType.wrongToken, "@#"),
    (TokenType.identifierToken, "a"),
    (TokenType.identifierToken, "ZCG"),
    (TokenType.identifierToken, "_12"),
    (TokenType.identifierToken, "__"),
    (TokenType.identifierToken, "_a1"),
    (TokenType.identifierToken, "_ab"),
    (TokenType.identifierToken, "a_b"),
    (TokenType.identifierToken, "a_12c"),
    (TokenType.identifierToken, "a_12"),
  )

  def testPairs(tokenType1: TokenType,
                text1: String,
                tokenType2: TokenType,
                text2: String): Boolean = {
    val lexer = Parser(SourceText(text1 + text2))
    val token1 = lexer.eat(tokenType1)
    val token2 = lexer.eat(tokenType2)
    if (token1.getKind == tokenType1
        && token2.getKind == tokenType2)
      return true
    false
  }



  def requireSeparator(type1: TokenType, type2: TokenType): Boolean = {
    val t1IsKeyword = type1.toString.endsWith("Keyword")
    val t2IsKeyword = type2.toString.endsWith("Keyword")

    (type1, type2, t1IsKeyword, t2IsKeyword) match {
      case (_, _, true, true)                        => true
      case (TokenType.`identifierToken`, _, _, true) => true
      case (_, TokenType.`identifierToken`, true, _) => true
      case (TokenType.literal, _, _, true)           => true
      case (_, TokenType.literal, true, _)           => true
      case (TokenType.`identifierToken`, TokenType.`identifierToken`, _, _) =>
        true
      case (TokenType.`identifierToken`, TokenType.literal, _, _) => true
      case (TokenType.literal, TokenType.literal, _, _)           => true
      case (TokenType.`equalsToken`, TokenType.`equalsEqualsToken`, _, _) =>
        true
      case (TokenType.`equalsEqualsToken`,
            TokenType.`equalsEqualsToken`,
            _,
            _) =>
        true
      case (TokenType.`lessToken`, TokenType.`equalsToken`, _, _) => true
      case (TokenType.`lessOrEqualsToken`, TokenType.`equalsToken`, _, _) =>
        true
      case (TokenType.`lessToken`, TokenType.`equalsEqualsToken`, _, _) => true
      case (TokenType.`greaterToken`, TokenType.`equalsToken`, _, _)    => true
      case (TokenType.`greaterOrEqualsToken`, TokenType.`equalsToken`, _, _) =>
        true
      case (TokenType.`greaterToken`, TokenType.`equalsEqualsToken`, _, _) =>
        true
      case (TokenType.`tildeToken`, TokenType.`equalsToken`, _, _)       => true
      case (TokenType.`tildeToken`, TokenType.`equalsEqualsToken`, _, _) => true
      case (TokenType.`equalsToken`, TokenType.`equalsToken`, _, _)      => true
      case (TokenType.`minusToken`, TokenType.`greaterOrEqualsToken`, _, _) =>
        true
      case (TokenType.`minusToken`, TokenType.`greaterToken`, _, _) => true
      case _                                                        => false
    }
  }

  def getTokenPairs: List[(TokenType, String, TokenType, String)] = {
    var list: List[(TokenType.TokenType, String, TokenType.TokenType, String)] =
      List()
    for (x <- tokens; y <- tokens)
      yield {
        if (!requireSeparator(x._1, y._1))
          list :+= (x._1, x._2, y._1, y._2)
      }
    list.distinct
  }

  def getTokenPairsWithSeparators
    : List[(TokenType, String, TokenType, String, TokenType, String)] = {
    var list: List[(TokenType.TokenType,
                    String,
                    TokenType.TokenType,
                    String,
                    TokenType,
                    String)] = List()
    for (x <- tokens; s <- separators; y <- tokens)
      yield {
        if (requireSeparator(x._1, y._1))
          list :+= (x._1, x._2, s._1, s._2, y._1, y._2)
      }
    list.distinct
  }

  (tokens ++ separators ++ others).foreach { x =>
    it should s"be a ${x._1}${new Random().nextInt()}" in {
      val token = Parser(SourceText(x._2)).eat(x._1)
      assert(x._1 == token.getKind)
    }
  }

  getTokenPairs.foreach { x =>
    it should s"be a ${x._1} and ${x._3} ${new Random().nextInt()}" in {
      assert(testPairs(x._1, x._2, x._3, x._4))
    }
  }

  getTokenPairsWithSeparators.foreach { x =>
    it should s"be a ${x._1} and ${x._5} ${new Random().nextInt()}" in {
      val lexer = Parser(SourceText(x._2 + x._4 + x._6))
      val token1 = lexer.eat(x._1)
      val token3 = lexer.eat(x._5)
      assert(token1.getKind == x._1)
      assert(token3.getKind == x._5)
      assert(token1.text == x._2)
      assert(token3.text == x._6)
    }
  }



}
