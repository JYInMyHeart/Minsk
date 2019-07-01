package lowering

import binder.{
  BindBinaryExpression,
  BindBlockStatement,
  BindConditionGotoStatement,
  BindForStatement,
  BindGotoStatement,
  BindIfStatement,
  BindLabel,
  BindLabelStatement,
  BindStatement,
  BindTreeRewriter,
  BindVariableDeclaration,
  BindVariableExpression,
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

  protected override def rewriteBindIfStatement(
      n: BindIfStatement): BindStatement = {
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

  protected override def rewriteForStatement(
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
