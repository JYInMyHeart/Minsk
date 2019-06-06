package symbol


object BuiltinFunctions {
  val mPrint: FunctionSymbol = new FunctionSymbol(
    "mPrint",
    List(new ParameterSymbol("text", TypeSymbol.String)),
    TypeSymbol.Void)
  val input: FunctionSymbol =
    new FunctionSymbol("input", List(), TypeSymbol.String)
  val rnd: FunctionSymbol = new FunctionSymbol(
    "rnd",
    List(new ParameterSymbol("max", TypeSymbol.Int)),
    TypeSymbol.Int)

  def getAll: List[FunctionSymbol] =
    BuiltinFunctions.getClass.getDeclaredFields
      .filter(_.getType.equals(classOf[FunctionSymbol]))
      .map(_.get(this).asInstanceOf[FunctionSymbol])
      .toList

}
