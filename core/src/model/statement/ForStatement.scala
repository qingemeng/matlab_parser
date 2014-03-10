package model.statement

import   model._
import   model.statement._
import   model.expression._
import   model.property._

object ForStatement {
  def apply(
      initStmt: Statement,
      condExpr: Option[Expr],
      iterExpr: Option[Expr],
      body: Statement) = new ForStatement(initStmt, condExpr, iterExpr, body)
//  def apply(
//             rangeExpr:Expr,
//             body: Statement) = new ForStatement2(rangeExpr, body)
  def unapply(s: ForStatement) = Some(s.initStmt, s.condExpr, s.iterExpr, s.body)
}

// initStmt can be either DeclarationStatement or ExpressionStatement
class ForStatement(
    private var _initStmt: Statement,
    private var _condExpr: Option[Expr],
    private var _iterExpr: Option[Expr],
    private var _body: Statement) extends Statement {

  update(_initStmt, _condExpr, _iterExpr, _body)

  def initStmt = _initStmt
  def condExpr = _condExpr
  def iterExpr = _iterExpr
  def body = _body

  def update(initStmt: Statement = _initStmt, condExpr: Option[Expr] = _condExpr, iterExpr: Option[Expr] = _iterExpr, body: Statement = _body) = {
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
      initStmt.cloneStmt(),
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

    if(newStmt.initStmt!=null){
      newStmt.initStmt.setParent(newStmt)
    }
    newStmt.body.setParent(newStmt)

    newStmt.base_copyFrom(this)
    newStmt
  }

  override def pretty() = pretty(0)
  override def pretty(level: Int): String = {
    val initStmtPretty = initStmt.pretty()
    val str = new StringBuilder
    str.append(indentStr(level))
    if(condExpr!=null &&iterExpr!=null){
      val condExprPretty = condExpr.map(_.pretty()).getOrElse("")
      val iterExprPretty = iterExpr.map(_.pretty()).getOrElse("")

      str.append(String.format("for (%s; %s; %s)\n", initStmtPretty, condExprPretty, iterExprPretty))


    }

    //if (props[LoopProperty].getCanAbstractLoop) str.append("can_abstr>")
    //else str.append("cannot_abstr>")
    else{
      str.append(String.format("for %s\n", initStmtPretty))
    }
    str.append(body.pretty(level + 1))
    str.toString
  }

  override def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("ForStatement: ")
    //str.append(pretty())
    str.append("\n")

    if (initStmt!=null){
      str.append(indentStr(level))
      str.append("->InitStmt: ")
      str.append(initStmt.pretty())
      str.append("\n")
      str.append(initStmt.treePretty(level+2))
    }

    if (condExpr!=null&&condExpr.isDefined){
      str.append(indentStr(level))
      str.append("->condExpr: ")
      str.append(condExpr.get.pretty())
      str.append("\n")
      str.append(condExpr.get.treePretty(level+2))
    }

    if (iterExpr!=null&&iterExpr.isDefined){
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

//  override def typePretty(level: Int = 0): String = {
//    val str = new StringBuilder
//    str.append(indentStr(level))
//    str.append("ForStatement: ")
//    //str.append(pretty())
//    str.append("\n")
//
//    if (initStmt!=null){
//      str.append(indentStr(level))
//      str.append("->InitStmt: ")
//      str.append(initStmt.pretty())
//      str.append("\n")
//      str.append(initStmt.typePretty(level+2))
//    }
//
//    if (condExpr!=null&&condExpr.isDefined){
//      str.append(indentStr(level))
//      str.append("->condExpr: ")
//      str.append(condExpr.get.pretty())
//      str.append("\n")
//      str.append(condExpr.get.typePretty(level+2))
//    }
//
//    if (iterExpr!=null&&iterExpr.isDefined){
//      str.append(indentStr(level))
//      str.append("->iterExpr: ")
//      str.append(iterExpr.get.pretty())
//      str.append("\n")
//      str.append(iterExpr.get.typePretty(level+2))
//    }
//
//    str.append(indentStr(level))
//    str.append("->ForBody:\n")
//    str.append(body.typePretty(level+2))
//
//    str.toString
//  }
}
