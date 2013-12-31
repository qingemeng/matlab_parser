package model.statement

import   model._
import   model.statement._
import   model.expression._

object FunctionCallStatement {
  def apply(funcCallExpr: FunctionCallExpr) = new FunctionCallStatement(funcCallExpr)
  def unapply(s: FunctionCallStatement) = Some(s.funcCallExpr)
}

class FunctionCallStatement(private var _funcCallExpr: FunctionCallExpr) extends Statement {
  
  update(_funcCallExpr)
  
  def funcCallExpr = _funcCallExpr
  
  def update(funcCallExpr: FunctionCallExpr = _funcCallExpr) = {
    _funcCallExpr = funcCallExpr
    this
  }
  
  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = FunctionCallStatement(funcCallExpr.cloneExpr.asInstanceOf[FunctionCallExpr])
    newStmt.base_copyFrom(this)
    newStmt
  }
  
  override def pretty(): String = funcCallExpr.pretty()
  
  override def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("FunctionCallStatement: ")
    //str.append(pretty())
    str.append("\n")
    
    str.append(funcCallExpr.treePretty(level+1))
    
    str.toString
  }
}