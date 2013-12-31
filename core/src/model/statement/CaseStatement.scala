package model.statement

import   model._
import   model.statement._
import   model.expression._

object CaseStatement {
  def apply(caseExpr: Expr) = new CaseStatement(caseExpr)
  def unapply(s: CaseStatement) = Some(s.caseExpr)
}

class CaseStatement(private var _caseExpr: Expr) extends Statement {
  
  def caseExpr = _caseExpr
  
  def update(caseExpr: Expr = _caseExpr) = {
    _caseExpr = caseExpr
    this
  }
  
  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = CaseStatement(caseExpr.cloneExpr)
    newStmt.base_copyFrom(this)
    newStmt
  }
  
  override def pretty(): String = {
    "case " + caseExpr.pretty() + ": "
  }
  
  override def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("CaseStatement: ")
    //str.append(PrettyPrinter.pretty(stmt))
    str.append("\n")
    
    str.append(caseExpr.treePretty(level+1))
    
    str.toString
  }
}