import scala.collection.mutable

class Compilation(ast: SyntaxTree,variables: mutable.HashMap[VariableSymbol,AnyVal]) {
  def evaluate(): EvaluationResult = {
    val binder = Binder(variables)
    val boundExpression = binder.bindExpression(ast.root)
    ast.diagnostics.concat(binder.diagnostics)
    val diagnostics = ast.diagnostics
    if (!diagnostics.isEmpty)
      return EvaluationResult(diagnostics, null.asInstanceOf[AnyVal])
    val evaluator = Eval(variables)
    val value = evaluator.eval(boundExpression)
    EvaluationResult(DiagnosticsBag(), value)
  }
}
