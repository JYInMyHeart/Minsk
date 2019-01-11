class Span(val line: Int,
           val column: Int) {

  override def toString: String = s"line $line,column $column"
}

object Span {
  def apply(line: Int,
            column: Int
           ): Span = new Span(
    line,
    column
  )
}
