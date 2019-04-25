import scala.collection.mutable

case class BoundScope(parent: BoundScope) {
  private val variables: mutable.HashMap[String, VariableSymbol] =
    mutable.HashMap()
  private val functions: mutable.HashMap[String, BindFuncStatement] =
    mutable.HashMap()

  def tryDeclare(variableSymbol: VariableSymbol): Boolean = {
    if (variables.contains(variableSymbol.name))
      false
    else {
      variables += variableSymbol.name -> variableSymbol
      true
    }
  }

  def tryDeclare(funcStatement: BindFuncStatement):Boolean = {
    if(functions.contains(funcStatement.identifier.name))
      false
    else{
      functions += funcStatement.identifier.name -> funcStatement
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

  def tryLookupFunc(name: String): BindFuncStatement = {
    if (functions.contains(name))
      return functions(name)
    if (parent == null)
      return null
    parent.tryLookupFunc(name)
  }

  def getDeclaredVariables: List[VariableSymbol] = {
    variables.values.toList
  }

  def getDeclaredFunctions: List[BindFuncStatement] = {
    functions.values.toList
  }
}

case class BoundGlobalScope(previous: BoundGlobalScope,
                            diagnostics: DiagnosticsBag,
                            variables: List[VariableSymbol],
                            functions:List[BindFuncStatement],
                            statement: BindStatement) {}
