import TokenType.TokenType

import scala.collection.mutable.ListBuffer

class Diagnostics(val span: Span, val message: String) {
  override def toString: String = message
}

object Diagnostics {
  def apply(span: Span, message: String): Diagnostics = new Diagnostics(
    span,
    message
  )
}

class DiagnosticsBag {
  def reportCannotAssign(span: Span, name: String): Unit = {
    val msg = s"Variable $name cannot be assigned at $span!"
    report(span, msg)
  }

  def reportVariableAlreadyDeclared(span: Span, name: String): Unit = {
    val msg = s"Variable $name has already declared at $span."
    report(span, msg)
  }

  def reportCannotConvert(span: Span,
                          bindTypeClass: String,
                          varType: String): Unit = {
    val msg =
      s"Cannot convert variable from $varType to $bindTypeClass at $span."
    report(span, msg)
  }

  val reports: ListBuffer[Diagnostics] = new ListBuffer[Diagnostics]()

  def report(span: Span, msg: String): reports.type = {
    reports += Diagnostics(span, msg)
  }

  def reportInvalidNumber(span: Span, text: String, clazz: String): Unit = {
    val msg = s"The number $text isn't valid $clazz at $span."
    report(span, msg)
  }

  def reportBadCharacter(position: Int, char: Char): Unit = {
    val span = Span(position, 0)
    val msg = s"Bad char input: '$char' at $span."
    report(span, msg)
  }

  def reportUnexpectedToken(span: Span,
                            actualType: TokenType,
                            expectedType: TokenType): Unit = {
    val msg =
      s"Unexpected token <$actualType>, expected <$expectedType> at $span."
    report(span, msg)
  }

  def reportUndefinedUnaryOperator(span: Span,
                                   operatorText: String,
                                   operandType: String): Unit = {
    val msg =
      s"Unary operator '$operatorText' is not defined for types $operandType at $span."
    report(span, msg)
  }

  def reportUndefinedBinaryOperator(span: Span,
                                    operatorText: String,
                                    leftType: String,
                                    rightType: String): Unit = {
    val msg =
      s"Binary operator '$operatorText' is not defined for types $leftType and $rightType at $span."
    report(span, msg)
  }

  def reportUndefinedName(span: Span, name: String): Unit = {
    val msg = s"Undefined variable $name at $span"
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
