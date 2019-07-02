package binder

import eval.DiagnosticsBag
import symbol.{FunctionSymbol, VariableSymbol, Symbol}

import scala.collection.mutable

case class BindScope(parent: BindScope) {
  private val symbols: mutable.HashMap[String, Symbol] =
    mutable.HashMap()

  def tryDeclareVariable(variableSymbol: VariableSymbol): Boolean =
    tryDeclareSymbol(variableSymbol)

  def tryDeclareFunction(functionSymbol: FunctionSymbol): Boolean =
    tryDeclareSymbol(functionSymbol)

  def tryDeclareSymbol[A <: Symbol](symbol: A): Boolean = {
    if (symbols.contains(symbol.name))
      false
    else {
      symbols += symbol.name -> symbol
      true
    }
  }

  def tryLookupVariable(name: String): VariableSymbol =
    tryLookupSymbol[VariableSymbol](name)

  def tryLookupFunction(name: String): FunctionSymbol =
    tryLookupSymbol[FunctionSymbol](name)

  def tryLookupSymbol[A <: Symbol](name: String): A = {
    if (symbols.contains(name)) {
      symbols(name) match {
        case x: A => return x
        case _    => return null.asInstanceOf[A]
      }
    }
    if (parent == null)
      return null.asInstanceOf[A]
    parent.tryLookupSymbol(name)
  }

  def getDeclaredVariables: List[VariableSymbol] = {
    symbols.values
      .filter(_.isInstanceOf[VariableSymbol])
      .map(_.asInstanceOf[VariableSymbol])
      .toList
  }

  def getDeclaredFunctions: List[FunctionSymbol] = {
    symbols.values
      .filter(_.isInstanceOf[FunctionSymbol])
      .map(_.asInstanceOf[FunctionSymbol])
      .toList
  }

}

case class BoundGlobalScope(previous: BoundGlobalScope,
                            diagnostics: DiagnosticsBag,
                            variables: List[VariableSymbol],
                            functions: List[FunctionSymbol],
                            statement: List[BindStatement]) {}
