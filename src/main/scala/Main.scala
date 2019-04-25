import scala.collection.mutable
import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = {
    import Printer._
    val variables = new mutable.HashMap[VariableSymbol, Any]()
    var showTree = false
    var previous: Compilation = null
    while (true) {
      print("> ")
      val str = StdIn.readLine()
      str match {
        case "q" => System.exit(0)
        case "show" =>
          showTree = !showTree
          if (showTree)
            println("show ast!")
          else
            println("not show ast!")
        case "h" =>
          println("""h:help
              |q:exit
              |show:show ast?
            """.stripMargin)
        case _ =>
          val tree = SyntaxTree.parse(str)
          if (showTree)
            prettyPrint(tree.root)
          if (tree.diagnostics.reports.nonEmpty) {
            tree.diagnostics.reports.foreach(
              x => colorPrintln(scala.io.AnsiColor.RED, x.toString)
            )
          } else {
            val compilation =
              if (previous == null)
                Compilation(tree, previous)
              else
                previous.continueWith(tree)
            val result = compilation.evaluate(variables)
            if (!result.diagnosticsBag.isEmpty) {
              result.diagnosticsBag.reports.foreach(
                x => colorPrintln(scala.io.AnsiColor.RED, x.toString)
              )
            } else {
              previous = compilation
              println(result.value)
            }
          }
      }
    }
  }
}
