package symbol

abstract class Symbol(name: String) {

  def kind: SymbolKind

  override def toString: String = name
}
