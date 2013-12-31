package model.statement

import   model._
import   model.statement._
import   model.expression._

object ContinueStatement {
  def apply() = {
    new ContinueStatement()
  }
}

class ContinueStatement extends Statement {
  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = ContinueStatement()
    newStmt.base_copyFrom(this)
    newStmt
  }
  
  override def pretty(): String = "continue;"
  override def treePretty(level: Int = 0): String = indentStr(level) + "ContinueStatement"
}