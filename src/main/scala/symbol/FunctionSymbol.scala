package symbol

class FunctionSymbol(val name: String,
                     val parameters: List[VariableSymbol],
                     val typeSymbol: TypeSymbol)
    extends Symbol(name) {
  override def kind: SymbolKind = SymbolKind.Function
}
