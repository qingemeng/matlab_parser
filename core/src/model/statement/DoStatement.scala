package model.statement

import   model._
import   model.statement._
import   model.expression._

object DoStatement {
  def apply(condExpr: Expr, body: Statement) = new DoStatement(condExpr, body)
  def unapply(s: DoStatement) = Some(s.condExpr, s.body)
}

class DoStatement(private var _condExpr: Expr, private var _body: Statement) extends Statement {
  
  update(_condExpr, _body)
  
  def condExpr = _condExpr
  def body = _body
  
  def update(condExpr: Expr = _condExpr, body: Statement = body) = {
    _condExpr = condExpr
    _body = body
    body.setParent(this)
    this
  }
  
  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = DoStatement(condExpr.cloneExpr(), body.cloneStmt())
    newStmt.body.setParent(newStmt)
    newStmt.base_copyFrom(this)
    newStmt  
  }
  
  override def pretty() = pretty(0)
  override def pretty(level: Int): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("do\n")
    str.append(body.pretty(level + 1))
    str.append("\n" + indentStr(level))
    str.append(String.format("while (%s)", condExpr.pretty()))
    str.toString
  }
  
  override def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("DoStatement: ")
    //str.append(PrettyPrinter.pretty(stmt))
    str.append("\n")
    
    str.append(indentStr(level))
    str.append("->Condition: ")
    str.append(condExpr.pretty())
    str.append("\n")
    str.append(condExpr.treePretty(level+2))
    
    str.append(indentStr(level))
    str.append("->DoBody: ")
    str.append(body.treePretty(level+2))
    str.toString
  }
}