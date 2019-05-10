import parser.TokenType.TokenType
import parser.{Node, Token}

import scala.collection.mutable

class AssertingEnumerator(enumerator: Iterator[Node]) {

  def assertNode(kind: TokenType): Unit = {
    assert(enumerator.hasNext)
    val current = enumerator.next()
    assert(kind == current.getKind)
    assert(!current.isInstanceOf[Token])
  }

  def assertToken(kind: TokenType, text: String): Unit = {
    assert(enumerator.hasNext)
    val current = enumerator.next()
    assert(kind == current.getKind)
    assert(current.isInstanceOf[Token])
    assert(text == current.asInstanceOf[Token].value)
  }

}
object AssertingEnumerator {

  def apply(enumerator: Iterator[Node]): AssertingEnumerator =
    new AssertingEnumerator(enumerator)
  def flatten(expression: Node): List[Node] = {
    var res = List[Node]()
    val stack: mutable.Stack[Node] = mutable.Stack()
    stack.push(expression)
    while (stack.nonEmpty) {
      val n = stack.pop()
      res :+= n
      if (n.getChildren != null)
        for (child <- n.getChildren.reverse)
          stack.push(child)
    }
    res
  }
}
