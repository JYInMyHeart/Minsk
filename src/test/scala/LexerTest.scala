import TokenType.TokenType

import scala.util.Random

class LexerTest extends UnitSpec {
  val dynamicTokens: List[(TokenType, String)] = List(
    //    (TokenType.keyword,""),
    //      (TokenType.func,""),

    (TokenType.identifier, "a_12"),
    (TokenType.literal, "1234"),
    (TokenType.eof, "\0"),
    (TokenType.falseKeyword, "false"),
    (TokenType.trueKeyword, "true")
  )

  val fixedTokens: List[(TokenType.Value, String)] =
    TokenType
      .values
      .map(k => (k,Facts.getText(k)))
      .filter(_._2 != null)
      .toList

  val tokens: List[(TokenType.Value, String)] = fixedTokens ++ dynamicTokens

  val separators = List(
    (TokenType.whiteSpace, " "),
    (TokenType.newline, "\n"),
  )

  val others = List(
    (TokenType.newline, "\r\n"),
    (TokenType.wrong, "@#"),
    (TokenType.identifier, "a"),
    (TokenType.identifier, "ZCG"),
    (TokenType.identifier, "_12"),
    (TokenType.identifier, "__"),
    (TokenType.identifier, "_a1"),
    (TokenType.identifier, "_ab"),
    (TokenType.identifier, "a_b"),
    (TokenType.identifier, "a_12c"),
    (TokenType.identifier, "a_12"),
  )


  def testPairs(tokenType1: TokenType,
                text1: String,
                tokenType2: TokenType,
                text2: String): Boolean = {
    val lexer = Lexer.newLexer(text1 + text2)
    val token1 = lexer.nextToken()
    val token2 = lexer.nextToken()
    if (token1.tokenType == tokenType1
      && token2.tokenType == tokenType2)
      return true
    false
  }


  def requireSeparator(type1: TokenType, type2: TokenType): Boolean = {
    val t1IsKeyword = type1.toString.endsWith("Keyword")
    val t2IsKeyword = type2.toString.endsWith("Keyword")

    (type1, type2, t1IsKeyword, t2IsKeyword) match {
      case (_, _, true, true) => true
      case (TokenType.identifier, _, _, true) => true
      case (_, TokenType.identifier, true, _) => true
      case (TokenType.literal, _, _, true) => true
      case (_, TokenType.literal, true, _) => true
      case (TokenType.identifier, TokenType.identifier, _, _) => true
      case (TokenType.identifier, TokenType.literal, _, _) => true
      case (TokenType.literal, TokenType.literal, _, _) => true
      case (TokenType.assign, TokenType.equal, _, _) => true
      case (TokenType.equal, TokenType.equal, _, _) => true
      case (TokenType.lt, TokenType.assign, _, _) => true
      case (TokenType.lte, TokenType.assign, _, _) => true
      case (TokenType.lt, TokenType.equal, _, _) => true
      case (TokenType.gt, TokenType.assign, _, _) => true
      case (TokenType.gte, TokenType.assign, _, _) => true
      case (TokenType.gt, TokenType.equal, _, _) => true
      case (TokenType.not, TokenType.assign, _, _) => true
      case (TokenType.not, TokenType.equal, _, _) => true
      case (TokenType.assign, TokenType.assign, _, _) => true
      case (TokenType.sub, TokenType.gte, _, _) => true
      case (TokenType.sub, TokenType.gt, _, _) => true
      case _ => false
    }
  }

  def getTokenPairs: List[(TokenType, String, TokenType, String)] = {
    var list: List[(TokenType.TokenType, String, TokenType.TokenType, String)] = List()
    tokens.foreach {
      x =>
        tokens.foreach {
          y =>
            if (!requireSeparator(x._1, y._1))
              list :+= (x._1, x._2, y._1, y._2)
        }
    }
    list.distinct
  }

  def getTokenPairsWithSeparators: List[(TokenType, String, TokenType, String, TokenType, String)] = {
    var list: List[(TokenType.TokenType, String, TokenType.TokenType, String, TokenType, String)] = List()
    tokens.foreach {
      x =>
        separators.foreach {
          s =>
            tokens.foreach {
              y =>
                if (requireSeparator(x._1, y._1))
                  list :+= (x._1, x._2, s._1, s._2, y._1, y._2)
            }
        }
    }
    list.distinct
  }


  (tokens ++ separators ++ others).foreach { x =>
    it should s"be a ${x._1}${new Random().nextInt()}" in {
      val token = Lexer.newLexer(x._2).nextToken()
      assert(x._1 == token.tokenType)
    }
  }

  getTokenPairs.foreach { x =>
    it should s"be a ${x._1} and ${x._3} ${new Random().nextInt()}" in {
      assert(testPairs(x._1, x._2, x._3, x._4))
    }
  }

  getTokenPairsWithSeparators.foreach{ x =>
    it should s"be a ${x._1} and ${x._3} ${new Random().nextInt()}" in {
      val lexer = Lexer.newLexer(x._2 + x._4 + x._6)
      val token1 = lexer.nextToken()
      val token2 = lexer.nextToken()
      val token3 = lexer.nextToken()
      assert(token1.tokenType == x._1)
      assert(token2.tokenType == x._3)
      assert(token3.tokenType == x._5)
      assert(token1.value == x._2)
      assert(token2.value == null)
      assert(token3.value == x._6)
    }

  }

}
