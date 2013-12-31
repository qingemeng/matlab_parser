package model.statement

import   model._
import   model.statement._
import   model.expression._

object ReturnStatement {
  def apply() = new ReturnStatement(None)
  def apply(returnValue: Expr) = new ReturnStatement(Some(returnValue))
  def unapply(s: ReturnStatement) = Some(s.returnValue)
}

class ReturnStatement(private var _returnValue: Option[Expr]) extends Statement {
  def returnValue = _returnValue
  
  def update(returnValue: Option[Expr] = _returnValue) = {
    _returnValue = returnValue
    this
  }

  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = returnValue match {
      case Some(expr) => ReturnStatement(expr.cloneExpr)
      case None 	  => ReturnStatement()
    } 
    newStmt.base_copyFrom(this)
    newStmt
  }
  
  override def pretty(): String = {
    returnValue match {
      case Some(expr) => "return " + expr.pretty()
      case None		  => "return"
    }
  }
  
  override def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("ReturnStatement: ")
    str.append(pretty())
    str.append("\n")
    
    returnValue match {
      case Some(expr) => str.append(expr.treePretty(level+1))
      case None		  => 
    }
    
    str.toString
  }
}