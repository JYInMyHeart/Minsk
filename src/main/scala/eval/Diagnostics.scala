package eval

import parser.Expression
import parser.TokenType.TokenType
import sourceText.TextSpan
import symbol.{TypeSymbol, VariableSymbol}

import scala.collection.mutable.ListBuffer

class Diagnostics(val span: TextSpan, val message: String) {
  override def toString: String = message
}

object Diagnostics {
  def apply(span: TextSpan, message: String): Diagnostics = new Diagnostics(
    span,
    message
  )
}

class DiagnosticsBag {
  def reportUndefinedType(text: String, getSpan: TextSpan) = {
    val msg = s"function type $text was undefined at $getSpan"
    report(getSpan,msg)
  }

  def reportParameterAlreadyDeclared(getSpan: TextSpan, parameterName: String) = {
    val msg = s"parameter $parameterName has already declared at $getSpan"
    report(getSpan,msg)
  }

  def reportUndefinedFunction(span: TextSpan, text: String) = {
    val msg = s"Function $text was undefined at $span"
    report(span,msg)
  }

  def reportUnterminatedString(span: TextSpan): Unit = {
    val msg = s"unterminated string at $span"
    report(span,msg)
  }

  def reportParamMismatch(span: TextSpan, param: List[VariableSymbol], expressions: List[Expression]) = {
    val msg = s"functionCall's paramList should be $param bug got expressions at $span!"
    report(span, msg)
  }

  def reportCannotAssign(span: TextSpan, name: String): Unit = {
    val msg = s"Variable $name cannot be assigned at $span!"
    report(span, msg)
  }

  def reportVariableAlreadyDeclared(span: TextSpan, name: String): Unit = {
    val msg = s"Variable $name has already declared at $span."
    report(span, msg)
  }

  def reportFunctionAlreadyDeclared(span: TextSpan, name: String): Unit = {
    val msg = s"Function $name has already declared at $span."
    report(span, msg)
  }
  def reportCannotConvert(span: TextSpan,
                          bindTypeClass: TypeSymbol,
                          varType: TypeSymbol): Unit = {
    val msg =
      s"Cannot convert variable from $varType to $bindTypeClass at $span."
    report(span, msg)
  }

  val reports: ListBuffer[Diagnostics] = new ListBuffer[Diagnostics]()

  def report(span: TextSpan, msg: String): reports.type = {
    reports += Diagnostics(span, msg)
  }

  def reportInvalidNumber(span: TextSpan, text: String, clazz: TypeSymbol): Unit = {
    val msg = s"The number $text isn't valid $clazz at $span."
    report(span, msg)
  }

  def reportBadCharacter(position: Int, char: Char): Unit = {
    val span = TextSpan(position, 0)
    val msg = s"Bad char input: '$char' at $span."
    report(span, msg)
  }

  def reportUnexpectedToken(span: TextSpan,
                            actualType: TokenType,
                            expectedType: TokenType): Unit = {
    val msg =
      s"Unexpected token <$actualType>, expected <$expectedType> at $span."
    report(span, msg)
  }

  def reportUndefinedUnaryOperator(span: TextSpan,
                                   operatorText: String,
                                   operandType: String): Unit = {
    val msg =
      s"Unary operator '$operatorText' is not defined for types $operandType at $span."
    report(span, msg)
  }

  def reportUndefinedBinaryOperator(span: TextSpan,
                                    operatorText: String,
                                    leftType: String,
                                    rightType: String): Unit = {
    val msg =
      s"Binary operator '$operatorText' is not defined for types $leftType and $rightType at $span."
    report(span, msg)
  }

  def reportUndefinedName(span: TextSpan, name: String): Unit = {
    val msg = s"Undefined variable $name at $span"
    report(span, msg)
  }

  def reportFunctionTypeMismatched(span: TextSpan,  funcName:String,actualType: TypeSymbol,
                                   expectedType: TypeSymbol): Unit = {
    val msg = s"Function $funcName expect $expectedType but got $actualType at $span"
    report(span, msg)
  }

  def reportFunctionParametersLengthMismatched(span: TextSpan,  funcName:String,funcParamLen: Int,
                                               realParamLen: Int): Unit = {
    val msg = s"Function $funcName expect parameters length is $funcParamLen but got $realParamLen at $span"
    report(span, msg)
  }

  def concat(diagnosticsBag: DiagnosticsBag): DiagnosticsBag = {
    reports ++= diagnosticsBag.reports
    val resDiagnosticsBag = DiagnosticsBag()
    resDiagnosticsBag.reports ++= reports
    resDiagnosticsBag
  }

  def isEmpty: Boolean = reports.isEmpty
}

object DiagnosticsBag {
  def apply(): DiagnosticsBag = new DiagnosticsBag()
}
