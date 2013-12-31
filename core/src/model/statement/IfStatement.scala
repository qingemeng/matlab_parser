package model.statement

import   model._
import   model.statement._
import   model.expression._

object IfStatement {
  def apply(
      condExpr: Expr, 
      thenBody: Statement, 
      elseBody: Statement) = new IfStatement(condExpr, thenBody, Some(elseBody))
  
  def apply(
      condExpr: Expr, 
      thenBody: Statement, 
      elseBody: Option[Statement] = None) = new IfStatement(condExpr, thenBody, elseBody)
  def unapply(s: IfStatement) = Some(s.condExpr, s.thenBody, s.elseBody)
}

class IfStatement(
    private var _condExpr: Expr, 
    private var _thenBody: Statement, 
    private var _elseBody: Option[Statement]) extends Statement {
  
  update(_condExpr, _thenBody, _elseBody)
  
  def condExpr = _condExpr
  def thenBody = _thenBody
  def elseBody = _elseBody
  
  def update(condExpr: Expr = _condExpr, thenBody: Statement = _thenBody, elseBody: Option[Statement] = _elseBody) = {
    _condExpr = condExpr
    _thenBody = thenBody
    _elseBody = elseBody
    thenBody.setParent(this)
    if (elseBody != None) elseBody.get.setParent(this)
    this
  }
  
  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = IfStatement(
      condExpr.cloneExpr,
      thenBody.cloneStmt,
      elseBody match {
        case Some(stmt)	=> Some(stmt.cloneStmt)
        case None		=> None
      }
    )
    
    newStmt.thenBody.setParent(newStmt)
    if(newStmt.elseBody.isDefined){
      newStmt.elseBody.get.setParent(newStmt)
    }
    
    newStmt.base_copyFrom(this)
    newStmt
  }
  
  override def pretty() = pretty(0)
  override def pretty(level: Int): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append(String.format("if (%s)\n", condExpr.pretty()))
    str.append(thenBody.pretty(level + 1))
    if (elseBody.isDefined) {
      str.append("\n" + indentStr(level))
      str.append("else\n")
      str.append(elseBody.get.pretty(level + 1))
    }
    str.toString
  }
  
  override def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("IfStatement: ")
    //str.append(pretty())
    str.append("\n")
    
    str.append(indentStr(level))
    str.append("->Condition: ")
    str.append(condExpr.pretty())
    str.append("\n")
    str.append(condExpr.treePretty(level+2))
    
    str.append(indentStr(level))
    str.append("->ThenBody:\n")
    str.append(thenBody.treePretty(level+2))

    if (elseBody.isDefined) {
	  str.append(indentStr(level))
	  str.append("->ElseBody:\n")
	  str.append(elseBody.get.treePretty(level+2))
    }
    str.toString
  }
}