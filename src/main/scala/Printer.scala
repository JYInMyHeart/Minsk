
import scala.io.AnsiColor.{BLUE, BOLD, GREEN, RESET}
object Printer {
  def colorPrint(colorType: String, text: String): Unit =
    print(s"$colorType$BOLD$text$RESET")

  def colorPrintln(colorType: String, text: String): Unit =
    colorPrint(colorType, text + "\r\n")


  def prettyPrint(node: Expression, indent: String = "", isLast: Boolean = true) {
    var indents = indent
    val enable = node.getChildren() != null
    val marker = if (isLast) "└──" else "├──"


    colorPrint(BLUE, indent)
    colorPrint(BLUE, marker)
    colorPrint(BLUE, node.getKind().toString)


    node match {
      case tokens: Tokens if tokens.value != null =>
        print(" ")
        colorPrint(GREEN, tokens.value)
      case _ =>
    }

    println()

    indents += (if (isLast) "    " else "│   ")


    if (enable) {
      val last = node.getChildren().last
      for (child <- node.getChildren())
        prettyPrint(child, indents, child == last)
    }

  }
}
