import scala.collection.mutable

case class Compilation(ast: SyntaxTree, previous: Compilation) {
  private var globalScope: BoundGlobalScope = getGlobalScope

  def getGlobalScope: BoundGlobalScope = {
    if (globalScope == null) {
      if (previous != null) {
        val globalScopeTmp =
          Binder.bindGlobalScope(previous.globalScope, ast.root)
        globalScope = globalScopeTmp
      } else {
        val globalScopeTmp = Binder.bindGlobalScope(null, ast.root)
        globalScope = globalScopeTmp
      }
    }
    globalScope
  }

  def continueWith(tree: SyntaxTree): Compilation = {
    Compilation(tree, this)
  }
  def evaluate(
      variables: mutable.HashMap[VariableSymbol, AnyVal]): EvaluationResult = {
    val diagnosticsBag = ast.diagnostics.concat(globalScope.diagnostics)
    if (!diagnosticsBag.isEmpty)
      return EvaluationResult(diagnosticsBag, null.asInstanceOf[AnyVal])
    val evaluator = Eval(variables)
    val value = evaluator.eval(globalScope.statement)
    EvaluationResult(DiagnosticsBag(), value)
  }
}
