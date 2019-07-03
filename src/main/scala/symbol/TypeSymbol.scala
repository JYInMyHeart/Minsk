package symbol
case class TypeSymbol(override val name: String) extends Symbol(name) {
  override def kind: SymbolKind = SymbolKind.Type
}
object TypeSymbol {

  val Error = new TypeSymbol("?")
  val Bool = new TypeSymbol("bool")
  val Int = new TypeSymbol("int")
  val Double = new TypeSymbol("double")
  val String = new TypeSymbol("string")
  val Void = new TypeSymbol("void")
  val Object = new TypeSymbol("object")

}
