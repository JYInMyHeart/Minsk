package binder

import eval.DiagnosticsBag
import symbol.{FunctionSymbol, VariableSymbol}

import scala.collection.mutable

case class BindScope(parent: BindScope) {
  private val symbols =
    mutable.HashMap[String,Symbol]()


  def tryDeclare(variableSymbol: VariableSymbol): Boolean = tryDeclareSymbol(variableSymbol)


  def tryDeclare(functionSymbol: FunctionSymbol):Boolean = tryDeclareSymbol(functionSymbol)

  def tryDeclareSymbol[A <: Symbol](symbol:A):Boolean = {
    if (symbols.contains(symbol.name))
      false
    else {
      symbols += symbol.name -> symbol
      true
    }
  }


  def tryLookupVariable(name: String): VariableSymbol = tryLookupSymbol[VariableSymbol](name)

  def tryLookupFunction(name: String): FunctionSymbol = tryLookupSymbol[FunctionSymbol](name)

  def tryLookupSymbol[A >: Symbol](name:String):A = {
    if (symbols.contains(name))
      return symbols(name)
    if (parent == null)
      return null
    parent.tryLookupSymbol(name)
  }

  def getDeclaredVariables: List[VariableSymbol] = {
    getDeclaredSymbols[VariableSymbol]()
  }

  def getDeclaredFunctions: List[FunctionSymbol] = {
    getDeclaredSymbols[FunctionSymbol]()
  }

  def getDeclaredSymbols[A >: Symbol]():List[A] ={
    symbols.values.toList
  }


}

case class BoundGlobalScope(previous: BoundGlobalScope,
                            diagnostics: DiagnosticsBag,
                            variables: List[VariableSymbol],
                            functions:List[FunctionSymbol],
                            statement: List[BindStatement]) {}
