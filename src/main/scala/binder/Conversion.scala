package binder

import symbol.TypeSymbol

sealed class Conversion(val exists: Boolean,
                        val isIdentity: Boolean,
                        val isImplicit: Boolean) {

  def isExplicit: Boolean = exists && !isImplicit

}

object Conversion {
  val None = new Conversion(false, false, false)
  val Identity = new Conversion(true, true, true)
  val Implicit = new Conversion(true, false, true)
  val Explicit = new Conversion(true, false, false)

  def classify(from: TypeSymbol, to: TypeSymbol) = {
    (from, to) match {
      case (a, b) if a == b                       => Conversion.Identity
      case (TypeSymbol.Bool, TypeSymbol.String)   => Conversion.Explicit
      case (TypeSymbol.Int, TypeSymbol.String)    => Conversion.Explicit
      case (TypeSymbol.Double, TypeSymbol.String) => Conversion.Explicit
      case (TypeSymbol.String, TypeSymbol.Bool)   => Conversion.Explicit
      case (TypeSymbol.String, TypeSymbol.Double) => Conversion.Explicit
      case (TypeSymbol.String, TypeSymbol.Int)    => Conversion.Explicit
      case _                                      => Conversion.None
    }
  }
}
