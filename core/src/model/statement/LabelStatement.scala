package model.statement

import   model._
import   model.statement._
import   model.expression._

object LabelStatement {
  def apply(label: IdName, nestedStmt: Statement) = new LabelStatement(label, nestedStmt)
  def unapply(s: LabelStatement) = Some(s.label, s.nestedStmt)
}

class LabelStatement(val label: IdName, private var _nestedStmt: Statement) extends Statement {

  update(_nestedStmt)
  
  def nestedStmt = _nestedStmt
  
  def update(nestedStmt: Statement = _nestedStmt) = {
    _nestedStmt = nestedStmt
    _nestedStmt.setParent(this)
    this
  }
  
  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = LabelStatement(label.cloneName, nestedStmt.cloneStmt)
    newStmt.nestedStmt.setParent(newStmt)
    newStmt.base_copyFrom(this)
    newStmt  
  }
  
  override def pretty(): String = label.pretty(false) + ": " + nestedStmt.pretty()
  
  override def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("LabelStatement: ")
    //str.append(pretty())
    str.append("\n")
    str.toString
  }
}