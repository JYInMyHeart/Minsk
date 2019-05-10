package symbol

import binder.BindStatement

case class VariableSymbol(name: String, varType: String, isReadOnly: Boolean) {}

case class FunctionSymbol(name: VariableSymbol,
                          parameters: List[VariableSymbol],
                          returnSymbol: VariableSymbol,
                          body: BindStatement)
