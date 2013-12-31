package model.statement

import   model._
import   model.statement._
import   model.expression._

object ExpressionStatement {
  def apply(expr: Expr) = new ExpressionStatement(expr)
  def unapply(s: ExpressionStatement) = Some(s.expr)
}

class ExpressionStatement(private var _expr: Expr) extends Statement {
  
  update(expr = _expr)
  
  def expr = _expr
  
  def update(expr: Expr = _expr) = {
    _expr = expr
    this
  }
  
  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = ExpressionStatement(expr.cloneExpr)
    newStmt.base_copyFrom(this)
    newStmt  
  }
  
  override def pretty(): String = expr.pretty()    
  
  override def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("ExpressionStatement: ")
    //str.append(pretty())
    str.append("\n")
    
    str.append(expr.treePretty(level+1))
    
    str.toString
  }
}