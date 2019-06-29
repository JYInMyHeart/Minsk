package symbol

import parser.FunctionDeclarationNode

class FunctionSymbol( name:String,
                      val parameters: List[ParameterSymbol],
                     val typeSymbol: TypeSymbol,
                     var functionDeclarationNode: FunctionDeclarationNode = null)
    extends Symbol(name) {
  override def kind: SymbolKind = SymbolKind.Function
}
