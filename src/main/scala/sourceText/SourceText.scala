package sourceText

import scala.collection.mutable.ListBuffer

class SourceText(text: String) {

  def lines: List[TextLine] = SourceText.parseLines(this, text)

  def length = text.length

  def getLineIndex(position: Int): Int = {
    var lower = 0
    var upper = lines.length - 1

    while (lower <= upper) {
      val index = lower + (upper - lower) / 2
      val start = lines(index).start

      if (position == start)
        return index

      if (start > position) {
        upper = index - 1
      } else {
        lower = index + 1
      }
    }

    lower
  }

  def at(index: Int): Char = text.charAt(index)

  override def toString: String = text

  def toString(start: Int, length: Int): String = text.substring(start, length)

  def toString(span: TextSpan): String = toString(span.start, span.length)
}

object SourceText {

  def getLineBreakWith(text: String, position: Int): Int = {
    val c = text(position)
    val l =
      if (position + 1 >= text.length) '\0'
      else text(position + 1)

    if (c == '\r' && l == '\n') return 2
    if (c == '\r' || c == '\n') return 1
    0
  }

  def addLine(result: ListBuffer[TextLine],
              sourceText: SourceText,
              position: Int,
              lineStart: Int,
              lineBreakWidth: Int): Unit = {
    val lineLength: Int = position - lineStart
    val lineLengthIncludingLineBreak: Int = lineLength + lineBreakWidth
    val line: TextLine =
      TextLine(sourceText, lineStart, lineLength, lineLengthIncludingLineBreak)
    result += line

  }

  def parseLines(sourceText: SourceText, text: String): List[TextLine] = {
    val result = ListBuffer[TextLine]()

    var position = 0
    var lineStart = 0
    while (position < text.length) {
      val lineBreakWidth = getLineBreakWith(text, position)
      lineBreakWidth match {
        case 0 => position += 1
        case _ =>
          addLine(result, sourceText, position, lineStart, lineBreakWidth)
          position += lineBreakWidth
          lineStart = position
      }
    }

    if (position >= lineStart)
      addLine(result, sourceText, position, lineStart, 0)
    result.toList
  }

  def apply(text: String): SourceText = new SourceText(text)
}
