package repl

import scala.collection.mutable.ListBuffer

class Repl {
  private[this] val submissionHistory: ListBuffer[String] = ListBuffer()
  private[this] var submissionHistroyIndex: Int = _
  private[this] var done: Boolean = _

  def evalateMetaCommand(text: String) = {
    println(s"Invald command ${text}")
  }

  def evalateSubmission(text: String) = ???

  def run(): Unit = {
    while (true) {
      val text: String = ""
      if (text == null || text.isEmpty)
        return
      if (!text.contains('\n') && text.startsWith("#"))
        evalateMetaCommand(text)
      else
        evalateSubmission(text)

      submissionHistory += text
      submissionHistroyIndex = 0
    }
  }

  sealed class SubmissionView(val lineRenderer: ListBuffer[String],
                              val submissionDocument: ListBuffer[String]) {

    var cursorTop: Int = _
    var renderedLineCount: Int = _
    var currentLine: Int = _
    var currentCharacter: Int = _

    def render(): Unit = {}

  }

}
