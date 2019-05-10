package symbol
class TypeSymbol(name:String) extends Symbol(name){
  override def kind: SymbolKind = SymbolKind.Type
}
object TypeSymbol {

  val Error = new TypeSymbol("?")
  val Bool = new TypeSymbol("bool")
  val Int = new TypeSymbol("int")
  val String = new TypeSymbol("string")
  val Void = new TypeSymbol("void")

}
