package symbol

abstract class Symbol(val name: String) {

  def kind: SymbolKind

  override def toString: String = name
}
