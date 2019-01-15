case class Span(line: Int,
                column: Int) {

  override def toString: String = s"line $line..column $column"
}


