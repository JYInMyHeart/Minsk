package sourceText

class TextSpan(val start:Int,
               val length:Int) {

}

object TextSpan{
  def apply( start: Int, length: Int): TextSpan = new TextSpan(start, length)
  def fromBounds(start:Int,end:Int) = new TextSpan(start,end - start)
}
