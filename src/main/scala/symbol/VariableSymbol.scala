package symbol

import binder.BindStatement

class VariableSymbol(val name: String, val typeSymbol: TypeSymbol, val isReadOnly: Boolean)
    extends Symbol(name) {
  override def kind: SymbolKind = SymbolKind.Variable
}
