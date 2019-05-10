package sourceText

class TextLine(val sourceText: SourceText,
               val start:Int,
               val length:Int,
               val lengthIncludingLineBreak:Int) {
  val end = start + length
  val span = TextSpan(start,length)
  val spanIncludingLineBreak = TextSpan(start,lengthIncludingLineBreak)
}

object TextLine{
  def apply(sourceText: SourceText,
            start: Int,
            length: Int,
            lengthIncludingLineBreak: Int): TextLine = new TextLine(sourceText, start, length, lengthIncludingLineBreak)
}
