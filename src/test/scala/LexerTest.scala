import TokenType.TokenType

import scala.util.Random

class LexerTest extends UnitSpec {
  val testData: List[(TokenType, String)] = List(
    //    (TokenType.keyword,""),
    //      (TokenType.func,""),
    (TokenType.identifier, "a"),
    (TokenType.identifier, "ZCG"),
    (TokenType.identifier, "_12"),
    (TokenType.identifier, "__"),
    (TokenType.identifier, "_a1"),
    (TokenType.identifier, "_ab"),
    (TokenType.identifier, "a_b"),
    (TokenType.identifier, "a_12c"),
    (TokenType.identifier, "a_12"),
    (TokenType.identifier, "a_12"),
    (TokenType.literal, "1"),
    (TokenType.literal, "1234"),
    (TokenType.lb, "("),
    (TokenType.rb, ")"),
    (TokenType.assign, "="),
    (TokenType.equal, "=="),
    (TokenType.notequal, "!="),
    (TokenType.lt, "<"),
    (TokenType.gt, ">"),
    (TokenType.lte, "<="),
    (TokenType.gte, ">="),
    (TokenType.add, "+"),
    (TokenType.sub, "-"),
    (TokenType.plus, "*"),
    (TokenType.div, "/"),
    (TokenType.mod, "%"),
    (TokenType.and, "&"),
    (TokenType.or, "|"),
    (TokenType.not, "!"),
    (TokenType.pow, "^"),
    (TokenType.whiteSpace, " "),
    (TokenType.newline, "\n"),
    (TokenType.newline, "\r\n"),
    (TokenType.eof, "\0"),
    (TokenType.wrong, "@#"),
    (TokenType.falseKeyword, "false"),
    (TokenType.trueKeyword, "true")
  )



  def testPairs(tokenType1: TokenType,
                text1:String,
                tokenType2: TokenType,
                text2:String): Boolean = {
    val lexer = Lexer.newLexer(text1 + text2)
    val token1 = lexer.nextToken()
    val token2 = lexer.nextToken()
    if(token1.tokenType == tokenType1
      && token2.tokenType == tokenType2)
      return true
    false
  }

  testData.foreach { x =>
    it should s"be a ${x._1}${new Random().nextInt()}" in {
      val token = Lexer.newLexer(x._2).nextToken()
      assert(x._1 == token.tokenType)
    }
  }

}
