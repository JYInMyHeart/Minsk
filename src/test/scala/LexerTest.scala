import TokenType.TokenType

import scala.util.Random

class LexerTest extends UnitSpec {
  val testData: List[(TokenType, String)] = List(
    //    (TokenType.keyword,""),
    //      (TokenType.func,""),
    (TokenType.identifier, "a"),
    (TokenType.identifier, "ZCG"),
    (TokenType.literal, "1"),
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

  testData.foreach { x =>
    it should s"be a ${x._1}${new Random().nextInt()}" in {
      val token = Lexer.newLexer(x._2).nextToken()
      assert(x._1 == token.tokenType)
    }
  }
}
