package model.statement

import   model._
import   model.statement._
import   model.expression._

object NullStatement {
  def apply() = {
    new NullStatement()
  }
}

class NullStatement extends Statement {
  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = NullStatement()
    newStmt.base_copyFrom(this)
    newStmt
  }
  
  override def pretty(): String = "Null Statement"
  override def treePretty(level: Int = 0): String = indentStr(level) + "NullStatement"
}