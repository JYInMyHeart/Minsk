import TokenType.TokenType

import scala.collection.mutable

class AssertingEnumerator(enumerator: Iterator[Ast]) {

  def assertNode(kind: TokenType): Unit = {
    assert(enumerator.hasNext)
    val current = enumerator.next()
    assert(kind == current.getKind)
    assert(!current.isInstanceOf[Tokens])
  }

  def assertToken(kind: TokenType, text: String): Unit = {
    assert(enumerator.hasNext)
    val current = enumerator.next()
    assert(kind == current.getKind)
    assert(current.isInstanceOf[Tokens])
    assert(text == current.asInstanceOf[Tokens].value)
  }

}
object AssertingEnumerator {

  def apply(enumerator: Iterator[Ast]): AssertingEnumerator =
    new AssertingEnumerator(enumerator)
  def flatten(expression: Ast): List[Ast] = {
    var res = List[Ast]()
    val stack: mutable.Stack[Ast] = mutable.Stack()
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
