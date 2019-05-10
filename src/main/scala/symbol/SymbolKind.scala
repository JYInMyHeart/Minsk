package symbol
class SymbolKind {}
object SymbolKind {

  val Function = new SymbolKind()
  val Variable = new SymbolKind()
  val Parameter = new SymbolKind()
  val Type = new SymbolKind()

}
