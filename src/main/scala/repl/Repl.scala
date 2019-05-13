package repl

import eval.Compilation
import parser.Printer.{colorPrintln, prettyPrint}
import parser.SyntaxTree
import symbol.VariableSymbol

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

class Repl {
  private[this] val submissionHistory:ListBuffer[String] = ListBuffer()
  private[this] var submissionHistroyIndex:Int = _
  private[this] var done:Boolean = _


  def evalateMetaCommand(text: String) = {
    println(s"Invald command ${text}")
  }

  def evalateSubmission(text: String) = ???

  def run():Unit = {
    while(true){
      val text:String = ""
      if(text == null || text.isEmpty)
        return
      if(!text.contains('\n') && text.startsWith("#"))
        evalateMetaCommand(text)
      else
        evalateSubmission(text)

      submissionHistory += text
      submissionHistroyIndex = 0
    }
  }


  sealed class SubmissionView(val lineRenderer:ListBuffer[String],
                              val submissionDocument:ListBuffer[String]){

    var cursorTop:Int = _
    var renderedLineCount:Int = _
    var currentLine:Int = _
    var currentCharacter:Int = _
    
    def render(): Unit = {
      
    }
    
  }





}
