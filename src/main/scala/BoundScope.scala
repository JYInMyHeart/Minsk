import scala.collection.mutable

case class BoundScope(parent: BoundScope) {
  private val variables: mutable.HashMap[String, VariableSymbol] =
    mutable.HashMap()

  def tryDeclare(variableSymbol: VariableSymbol): Boolean = {
    if (variables.contains(variableSymbol.name))
      false
    else {
      variables += variableSymbol.name -> variableSymbol
      true
    }
  }

  def tryLookup(name: String): VariableSymbol = {
    if (variables.contains(name))
      return variables(name)
    if (parent == null)
      return null
    parent.tryLookup(name)
  }

  def getDeclaredVariables: List[VariableSymbol] = {
    variables.values.toList
  }
}

case class BoundGlobalScope(previous: BoundGlobalScope,
                            diagnostics: DiagnosticsBag,
                            variables: List[VariableSymbol],
                            statement: BindStatement) {}
