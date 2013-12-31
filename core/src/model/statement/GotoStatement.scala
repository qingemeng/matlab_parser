package model.statement

import   model._
import   model.statement._
import   model.expression._

object GotoStatement {
  def apply(label: IdName) = new GotoStatement(label)
  def unapply(s: GotoStatement) = Some(s.label)
}

class GotoStatement(val label: IdName) extends Statement {
  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = GotoStatement(label.cloneName)
    newStmt.base_copyFrom(this)
    newStmt
  }
  
  override def pretty(): String = "goto " + label.pretty(false)
  
  override def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("GotoStatement: ")
    //str.append(pretty())
    str.append("\n")
    str.toString
  }
}