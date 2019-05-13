

import eval.Compilation
import parser.Printer.{colorPrintln, prettyPrint}
import parser.SyntaxTree
import sourceText.SourceText
import symbol.VariableSymbol

import scala.collection.mutable
import scala.io.{Source, StdIn}

object Main {
  import parser.Printer._
  val variables = new mutable.HashMap[VariableSymbol, Any]()
  var showTree = false
  var previous: Compilation = _
  def evalResult(str: String): Unit = {
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

  def loadFile(path: String): Unit = {
    val realPath = this.getClass.getResource(path).getPath
    val file = Source.fromFile(realPath, "UTF-8")
    val source = file.mkString
    evalResult(source)
  }

  def commandLine(): Unit = {
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
          evalResult(str)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    commandLine()

  }
}
