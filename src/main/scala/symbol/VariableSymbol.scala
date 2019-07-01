package symbol

class VariableSymbol(name: String,
                     val typeSymbol: TypeSymbol,
                     val isReadOnly: Boolean)
    extends Symbol(name) {
  override def kind: SymbolKind = SymbolKind.Variable
}

class LocalVariableSymbol(name: String,
                          typeSymbol: TypeSymbol,
                          isReadOnly: Boolean)
    extends VariableSymbol(name, typeSymbol, isReadOnly) {
  override def kind: SymbolKind = SymbolKind.LocalVariable
}
object LocalVariableSymbol {
  def apply(name: String,
            typeSymbol: TypeSymbol,
            isReadOnly: Boolean): LocalVariableSymbol =
    new LocalVariableSymbol(name, typeSymbol, isReadOnly)
}
