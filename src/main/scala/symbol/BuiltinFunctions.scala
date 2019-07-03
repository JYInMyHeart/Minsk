package symbol

object BuiltinFunctions {
  val mPrint: FunctionSymbol = new FunctionSymbol(
    "mPrint",
    List(new ParameterSymbol("text", TypeSymbol.String)),
    TypeSymbol.Void)
  val input: FunctionSymbol =
    new FunctionSymbol("input", List(), TypeSymbol.String)
  val inputInt: FunctionSymbol =
    new FunctionSymbol("inputInt", List(), TypeSymbol.Int)
  val inputDouble: FunctionSymbol =
    new FunctionSymbol("inputDouble", List(), TypeSymbol.Double)
  val inputBool: FunctionSymbol =
    new FunctionSymbol("inputBool", List(), TypeSymbol.Bool)
  val rnd: FunctionSymbol = new FunctionSymbol(
    "rnd",
    List(new ParameterSymbol("max", TypeSymbol.Int)),
    TypeSymbol.Int)

  val doubleToStr: FunctionSymbol = new FunctionSymbol(
    "doubleToStr",
    List(
      new ParameterSymbol("str", TypeSymbol.Double)
    ),
    TypeSymbol.String
  )

  val intToStr: FunctionSymbol = new FunctionSymbol(
    "intToStr",
    List(
      new ParameterSymbol("int", TypeSymbol.Int)
    ),
    TypeSymbol.String
  )

  val boolToStr: FunctionSymbol = new FunctionSymbol(
    "boolToStr",
    List(
      new ParameterSymbol("bool", TypeSymbol.Bool)
    ),
    TypeSymbol.String
  )

  def getAll: List[FunctionSymbol] =
    BuiltinFunctions.getClass.getDeclaredFields
      .filter(_.getType.equals(classOf[FunctionSymbol]))
      .map(_.get(this).asInstanceOf[FunctionSymbol])
      .toList

}
