import parser.Facts._
import parser.{SyntaxTree, TokenType}

import scala.util.Random

class ParserTest extends UnitSpec {
  def getBinaryOperatorPairsData: List[(TokenType.Value, TokenType.Value)] = {
    for (o1 <- getBinaryOperatorKinds; o2 <- getBinaryOperatorKinds)
      yield (o1, o2)
  }

  def getUnaryOperatorPairsData: List[(TokenType.Value, TokenType.Value)] = {
    for (o1 <- getUnaryOperatorKinds; o2 <- getBinaryOperatorKinds)
      yield (o1, o2)
  }

  def parseBinaryExpressionHonorsPrecedences(op1: TokenType.TokenType,
                                             op2: TokenType.TokenType): Unit = {
    val op1Precedence = getBinaryOperatorPrecedence(op1)
    val op2Precedence = getBinaryOperatorPrecedence(op2)
    val op1Text = getText(op1)
    val op2Text = getText(op2)
    val expression = SyntaxTree.parse(s"a $op1Text b $op2Text c")
    if (op1Precedence >= op2Precedence) {
      val e = AssertingEnumerator(
        AssertingEnumerator.flatten(expression.root).iterator)

      e.assertNode(TokenType.compilationUnit)
      e.assertNode(TokenType.globalStatement)
      e.assertNode(TokenType.expressionStatement)
      e.assertNode(TokenType.binaryExpression)
      e.assertNode(TokenType.binaryExpression)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifierToken, "a")
      e.assertToken(op1, op1Text)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifierToken, "b")
      e.assertToken(op2, op2Text)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifierToken, "c")
    } else {
      val e = AssertingEnumerator(
        AssertingEnumerator.flatten(expression.root).iterator)

      e.assertNode(TokenType.compilationUnit)
      e.assertNode(TokenType.globalStatement)
      e.assertNode(TokenType.expressionStatement)
      e.assertNode(TokenType.binaryExpression)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifierToken, "a")
      e.assertToken(op1, op1Text)
      e.assertNode(TokenType.binaryExpression)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifierToken, "b")
      e.assertToken(op2, op2Text)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifierToken, "c")
    }
  }

  def parseUnaryExpressionHonorsPrecedences(un: TokenType.TokenType,
                                            bin: TokenType.TokenType): Unit = {
    val unPrecedence = getUnaryOperatorPrecedence(un)
    val binPrecedence = getBinaryOperatorPrecedence(bin)
    val unText = getText(un)
    val binText = getText(bin)
    val expression = SyntaxTree.parse(s"$unText a $binText b ")
    if (unPrecedence >= binPrecedence) {
      val e = AssertingEnumerator(
        AssertingEnumerator.flatten(expression.root).iterator)
      e.assertNode(TokenType.compilationUnit)
      e.assertNode(TokenType.globalStatement)
      e.assertNode(TokenType.expressionStatement)
      e.assertNode(TokenType.binaryExpression)
      e.assertNode(TokenType.unaryExpression)
      e.assertToken(un, unText)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifierToken, "a")
      e.assertToken(bin, binText)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifierToken, "b")
    } else {
      val e = AssertingEnumerator(
        AssertingEnumerator.flatten(expression.root).iterator)
      e.assertNode(TokenType.compilationUnit)
      e.assertNode(TokenType.globalStatement)
      e.assertNode(TokenType.expressionStatement)
      e.assertNode(TokenType.unaryExpression)
      e.assertToken(un, unText)
      e.assertNode(TokenType.binaryExpression)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifierToken, "a")
      e.assertToken(bin, binText)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifierToken, "b")
    }
  }

  getBinaryOperatorPairsData.foreach { x =>
    it should s"be a binaryTree ${x._1} and ${x._2}  ${new Random().nextInt()}" in {
      parseBinaryExpressionHonorsPrecedences(x._1, x._2)
    }
  }

//  it should "nice" in {
//    parseBinaryExpressionHonorsPrecedences(TokenType.pipeToken,
//                                           TokenType.minusToken)
//  }

  getUnaryOperatorPairsData.foreach { x =>
    it should s"be a unaryTree ${x._1} and ${x._2}  ${new Random().nextInt()}" in {
      parseUnaryExpressionHonorsPrecedences(x._1, x._2)
    }
  }

}
