package lowering

import binder.{
  BindAssignmentExpression,
  BindBinaryExpression,
  BindBlockStatement,
  BindConditionGotoStatement,
  BindExpressionStatement,
  BindForStatement,
  BindGotoStatement,
  BindIfStatement,
  BindLabel,
  BindLabelStatement,
  BindLiteralExpression,
  BindStatement,
  BindTreeRewriter,
  BindVariableDeclaration,
  BindVariableExpression,
  BindWhileStatement,
  BoundBinaryOperator
}
import parser.TokenType
import symbol.{LocalVariableSymbol, TypeSymbol}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Lowerer extends BindTreeRewriter {
  private var labelCount: Int = _

  private def generateLabel(): BindLabel = {
    val name = s"Label${labelCount}"
    labelCount += 1
    BindLabel(name)
  }

  override def rewriteBindIfStatement(n: BindIfStatement): BindStatement = {
    if (n.elseStatement == null) {
      val endLabel = generateLabel()
      val gotoFalse =
        BindConditionGotoStatement(endLabel, n.condition, jumpIfTrue = false)
      val endLabelStatement = BindLabelStatement(endLabel)
      val result = BindBlockStatement(
        List(gotoFalse, n.thenStatement, endLabelStatement))
      rewriteStatement(result)
    } else {
      val elseLabel = generateLabel()
      val endLabel = generateLabel()
      val gotoFalse =
        BindConditionGotoStatement(endLabel, n.condition, jumpIfTrue = false)
      val gotoEndStatement = BindGotoStatement(endLabel)
      val elseLabelStatement = BindLabelStatement(elseLabel)
      val endLabelStatement = BindLabelStatement(endLabel)
      val result = BindBlockStatement(
        List(gotoFalse,
             n.thenStatement,
             gotoEndStatement,
             elseLabelStatement,
             n.elseStatement,
             endLabelStatement))
      rewriteStatement(result)
    }
  }

  override def rewriteBindWhileStatement(
      node: BindWhileStatement): BindStatement = {
    val bodyLabel = generateLabel()
    val gotoContinue = BindGotoStatement(node.continueLabel)
    val bodyLabelStatement = BindLabelStatement(bodyLabel)
    val continueLabelStatement = BindLabelStatement(node.continueLabel)
    val gotoTrue = BindConditionGotoStatement(bodyLabel, node.condition)
    val breakLabelStatement = BindLabelStatement(node.breakLabel)

    val result = BindBlockStatement(
      List(
        gotoContinue,
        bodyLabelStatement,
        node.body,
        continueLabelStatement,
        gotoTrue,
        breakLabelStatement
      ))
    rewriteStatement(result)
  }

  override def rewriteBindForStatement(
      node: BindForStatement): BindStatement = {
    val variableDeclaration =
      BindVariableDeclaration(node.variable, node.initializer)
    val variableExpression = BindVariableExpression(node.variable)
    val upperBoundSymbol =
      LocalVariableSymbol("upperBound", TypeSymbol.Int, isReadOnly = true)
    val upperBoundDeclaration =
      BindVariableDeclaration(upperBoundSymbol, node.upper)
    val condition = BindBinaryExpression(
      BoundBinaryOperator.bind(
        TokenType.lessOrEqualsToken,
        TypeSymbol.Int,
        TypeSymbol.Int
      ),
      variableExpression,
      BindVariableExpression(upperBoundSymbol)
    )
    val continueLabelStatement = BindLabelStatement(node.continueLabel)
    val increment = BindExpressionStatement(
      BindAssignmentExpression(
        node.variable,
        BindBinaryExpression(
          BoundBinaryOperator.bind(TokenType.plusToken,
                                   TypeSymbol.Int,
                                   TypeSymbol.Int),
          variableExpression,
          BindLiteralExpression(1)
        )
      )
    )
    val whileBody = BindBlockStatement(
      List(
        node.body,
        continueLabelStatement,
        increment
      )
    )
    val whileStatement = BindWhileStatement(condition,
                                            whileBody,
                                            node.breakLabel,
                                            node.continueLabel)
    val result = BindBlockStatement(
      List(
        variableDeclaration,
        upperBoundDeclaration,
        whileStatement
      ))
    rewriteStatement(result)
  }
}

object Lowerer {
  def lower(statement: BindStatement): BindBlockStatement = {
    val lowerer = new Lowerer()
    val result = lowerer.rewriteStatement(statement)
    flatten(result)
  }

  private def flatten(statement: BindStatement) = {
    val builder = ListBuffer[BindStatement]()
    val stack = mutable.Stack[BindStatement]()
    stack.push(statement)
    while (stack.nonEmpty) {
      val current = stack.pop()
      current match {
        case block: BindBlockStatement =>
          for (s <- block.bindStatements.reverse)
            stack.push(s)
        case _ =>
          builder += current
      }
    }
    BindBlockStatement(builder.toList)
  }

}
