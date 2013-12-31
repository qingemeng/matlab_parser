package model.statement

import   model._
import   model.statement._
import   model.expression._

object DefaultStatement {
  def apply() = {
    new DefaultStatement()
  }
}

class DefaultStatement extends Statement {
  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = DefaultStatement()
    newStmt.base_copyFrom(this)
    newStmt
  }
  
  override def pretty(): String = "default: "
  override def treePretty(level: Int = 0): String = indentStr(level) + "DefaultStatement"
}