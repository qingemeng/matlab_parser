package model.statement

import   model._
import   model.statement._
import   model.expression._
import   model.property._

object ForStatement {
  def apply(
      initStmt: Option[Statement], 
      condExpr: Option[Expr], 
      iterExpr: Option[Expr], 
      body: Statement) = new ForStatement(initStmt, condExpr, iterExpr, body)
  def unapply(s: ForStatement) = Some(s.initStmt, s.condExpr, s.iterExpr, s.body)
}

// initStmt can be either DeclarationStatement or ExpressionStatement
class ForStatement(
    private var _initStmt: Option[Statement], 
    private var _condExpr: Option[Expr], 
    private var _iterExpr: Option[Expr], 
    private var _body: Statement) extends Statement {
  
  update(_initStmt, _condExpr, _iterExpr, _body)
  
  def initStmt = _initStmt
  def condExpr = _condExpr
  def iterExpr = _iterExpr
  def body = _body
  
  def update(initStmt: Option[Statement] = _initStmt, condExpr: Option[Expr] = _condExpr, iterExpr: Option[Expr] = _iterExpr, body: Statement = _body) = {
    _initStmt = initStmt
    _condExpr = condExpr
    _iterExpr = iterExpr
    _body = body
    _body.setParent(this)
    this
  }
  
  // deep clone of the model.statement
  override def cloneStmt() = cloneStmt(true)
  
  def cloneStmt(cloneBody: Boolean) = {
    val newStmt = ForStatement(
      initStmt match {
        case Some(stmt)	=> Some(stmt.cloneStmt)
        case None		=> None
      },
      condExpr match {
        case Some(expr)	=> Some(expr.cloneExpr)
        case None		=> None
      },
      iterExpr match {
        case Some(expr)	=> Some(expr.cloneExpr)
        case None		=> None
      },
      
      if(cloneBody){
    	body.cloneStmt()
      } else {
        StatementBlock()
      }
    )
    
    if(newStmt.initStmt.isDefined){
      newStmt.initStmt.get.setParent(newStmt)
    }
    newStmt.body.setParent(newStmt)
    
    newStmt.base_copyFrom(this)
    newStmt
  }
  
  override def pretty() = pretty(0)
  override def pretty(level: Int): String = {
    val initStmtPretty = initStmt.map(_.pretty()).getOrElse("")
    val condExprPretty = condExpr.map(_.pretty()).getOrElse("")
    val iterExprPretty = iterExpr.map(_.pretty()).getOrElse("")
    val str = new StringBuilder
    //if (props[LoopProperty].getCanAbstractLoop) str.append("can_abstr>")
    //else str.append("cannot_abstr>")
    str.append(indentStr(level))
    str.append(String.format("for (%s; %s; %s)\n", initStmtPretty, condExprPretty, iterExprPretty))
    str.append(body.pretty(level + 1))
    str.toString
  }
  
  override def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("ForStatement: ")
    //str.append(pretty())
    str.append("\n")
    
    if (initStmt.isDefined){
      str.append(indentStr(level))
      str.append("->InitStmt: ")
      str.append(initStmt.get.pretty())
      str.append("\n")
      str.append(initStmt.get.treePretty(level+2))
    }
    
    if (condExpr.isDefined){
      str.append(indentStr(level))
      str.append("->condExpr: ")
      str.append(condExpr.get.pretty())
      str.append("\n")
      str.append(condExpr.get.treePretty(level+2))
    }
    
    if (iterExpr.isDefined){
      str.append(indentStr(level))
      str.append("->iterExpr: ")
      str.append(iterExpr.get.pretty())
      str.append("\n")  
      str.append(iterExpr.get.treePretty(level+2))
    }
    
    str.append(indentStr(level))
    str.append("->ForBody:\n")
    str.append(body.treePretty(level+2))

    str.toString
  }
}
