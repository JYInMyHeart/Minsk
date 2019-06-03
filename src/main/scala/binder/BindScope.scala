package binder

import eval.DiagnosticsBag
import symbol.{FunctionSymbol, VariableSymbol}

import scala.collection.mutable

case class BindScope(parent: BindScope) {
  private val variables: mutable.HashMap[String, VariableSymbol] =
    mutable.HashMap()
  private val functions: mutable.HashMap[String, FunctionSymbol] =
    mutable.HashMap()

  def tryDeclare(variableSymbol: VariableSymbol): Boolean = {
    if (variables.contains(variableSymbol.name))
      false
    else {
      variables += variableSymbol.name -> variableSymbol
      true
    }
  }

  def tryDeclare(funcStatement: FunctionSymbol):Boolean = {
    if(functions.contains(funcStatement.name))
      false
    else{
      functions += funcStatement.name -> funcStatement
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

  def tryLookupFunc(name: String): FunctionSymbol = {
    if (functions.contains(name))
      return functions(name)
    if (parent == null)
      return null
    parent.tryLookupFunc(name)
  }

  def getDeclaredVariables: List[VariableSymbol] = {
    variables.values.toList
  }

  def getDeclaredFunctions: List[FunctionSymbol] = {
    functions.values.toList
  }
}

case class BoundGlobalScope(previous: BoundGlobalScope,
                            diagnostics: DiagnosticsBag,
                            variables: List[VariableSymbol],
                            functions:List[FunctionSymbol],
                            statement: BindStatement) {}
