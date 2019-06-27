package symbol

class ParameterSymbol(name: String, typeSymbol: TypeSymbol)
    extends VariableSymbol(name, typeSymbol, true) {}

object ParameterSymbol{
  def apply(name: String, typeSymbol: TypeSymbol): ParameterSymbol = new ParameterSymbol(name, typeSymbol)
}
