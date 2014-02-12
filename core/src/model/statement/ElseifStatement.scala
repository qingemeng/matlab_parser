package model.statement

import   model._
import   model.statement._
import   model.expression._

object ElseifStatement {
  def apply(
             condExpr: Expr,
             body: Statement) = new ElseifStatement(condExpr,body)
  def unapply(s: ElseifStatement) = Some(s.condExpr, s.body)
}

class ElseifStatement(
                   private var _condExpr: Expr,
                   private var _body: Statement)  extends Statement
{
  update(_condExpr, _body)

  def condExpr = _condExpr
  def body = _body

  def update(condExpr: Expr = _condExpr, body: Statement = _body) = {
    _condExpr = condExpr
    _body = body
    body.setParent(this)
    this
  }

  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = IfStatement(
      condExpr.cloneExpr,
      body.cloneStmt
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
    str.append(String.format("%s", condExpr.pretty()))
    str.append(body.pretty(level + 1))
    str.toString
  }

  override def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("ElseifStatement: ")
    //str.append(pretty())
    str.append("\n")

    str.append(indentStr(level))
    str.append("->ElseifCondition: ")
    str.append(condExpr.pretty())
    str.append("\n")
    str.append(condExpr.treePretty(level+2))

    str.append(indentStr(level))
    str.append("->ElseifBody:\n")
    str.append(body.treePretty(level+2))
    str.toString
  }
}