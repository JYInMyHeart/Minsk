import Facts._

import scala.util.Random
class ParserTest extends UnitSpec {
  def getBinaryOperatorPairsData:List[(TokenType.Value,TokenType.Value)] = {
    for(o1 <- getBinaryOperatorKinds;o2 <- getBinaryOperatorKinds)
      yield (o1,o2)
  }

  def getUnaryOperatorPairsData:List[(TokenType.Value,TokenType.Value)] = {
    for(o1 <- getUnaryOperatorKinds;o2 <- getBinaryOperatorKinds)
      yield (o1,o2)
  }


  def parseBinaryExpressionHonorsPrecedences(op1:TokenType.TokenType,op2:TokenType.TokenType): Unit = {
    val op1Precedence = getBinaryOperatorPrecedence(op1)
    val op2Precedence = getBinaryOperatorPrecedence(op2)
    val op1Text = getText(op1)
    val op2Text = getText(op2)
    val expression = SyntaxTree.parse(s"a $op1Text b $op2Text c")
    if(op1Precedence >= op2Precedence){
      val e = AssertingEnumerator(AssertingEnumerator.flatten(expression.root).iterator)
      e.assertNode(TokenType.expressionTree)
      e.assertNode(TokenType.binaryExpression)
      e.assertNode(TokenType.binaryExpression)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifier,"a")
      e.assertToken(op1,op1Text)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifier,"b")
      e.assertToken(op2,op2Text)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifier,"c")
    }else{
      val e = AssertingEnumerator(AssertingEnumerator.flatten(expression.root).iterator)
      e.assertNode(TokenType.expressionTree)
      e.assertNode(TokenType.binaryExpression)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifier,"a")
      e.assertToken(op1,op1Text)
      e.assertNode(TokenType.binaryExpression)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifier,"b")
      e.assertToken(op2,op2Text)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifier,"c")
    }
  }

  def parseUnaryExpressionHonorsPrecedences(un:TokenType.TokenType, bin:TokenType.TokenType): Unit = {
    val unPrecedence = getUnaryOperatorPrecedence(un)
    val binPrecedence = getBinaryOperatorPrecedence(bin)
    val unText = getText(un)
    val binText = getText(bin)
    val expression = SyntaxTree.parse(s"a $unText b $binText")
    if(unPrecedence >= binPrecedence){
      val e = AssertingEnumerator(AssertingEnumerator.flatten(expression.root).iterator)
      e.assertNode(TokenType.expressionTree)
      e.assertNode(TokenType.binaryExpression)
      e.assertNode(TokenType.unaryExpression)
      e.assertToken(un,unText)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifier,"a")
      e.assertToken(bin,binText)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifier,"b")
    }else{
      val e = AssertingEnumerator(AssertingEnumerator.flatten(expression.root).iterator)
      e.assertNode(TokenType.expressionTree)
      e.assertNode(TokenType.unaryExpression)
      e.assertToken(un,unText)
      e.assertNode(TokenType.binaryExpression)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifier,"a")
      e.assertToken(bin,binText)
      e.assertNode(TokenType.nameExpression)
      e.assertToken(TokenType.identifier,"b")
    }
  }

  getBinaryOperatorPairsData.foreach{ x =>
    it should s"be a binaryTree ${new Random().nextInt()}" in {
      parseBinaryExpressionHonorsPrecedences(x._1,x._2)
    }
  }

  // TODO: unaryExpression only will be parse one time.
//  getUnaryOperatorPairsData.foreach{ x =>
//    it should s"be a unaryTree ${new Random().nextInt()}" in {
//      parseUnaryExpressionHonorsPrecedences(x._1,x._2)
//    }
//  }


}
