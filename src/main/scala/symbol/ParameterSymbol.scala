package symbol

class ParameterSymbol(name: String, typeSymbol: TypeSymbol)
    extends VariableSymbol(name, typeSymbol, true) {}
