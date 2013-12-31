package model.statement

import   model._
import   model.statement._
import   model.expression._

object BreakStatement {
  def apply() = {
    new BreakStatement()
  }
}

class BreakStatement extends Statement {
  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = BreakStatement()
    newStmt.base_copyFrom(this)
    newStmt
  }
  
  override def pretty(): String = "break;"
  
  override def treePretty(level: Int = 0): String = indentStr(level) + "BreakStatement"
}