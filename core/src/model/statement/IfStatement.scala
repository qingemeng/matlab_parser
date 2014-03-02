package model.statement

import   model._
import   model.statement._
import   model.expression._

object IfStatement {
//    def apply(
//               condExpr: Expr,
//               thenBody: Statement,
//
//               elseifStmts: StatementBlock,
//               elseBody: Statement) = new IfStatement(condExpr, thenBody,Some(elseifStmts),Some(elseBody))


//  def apply(
//             condExpr: Expr,
//             thenBody: Statement,
//             elseBody: Statement) = new IfStatement(condExpr, thenBody, null,Some(elseBody))
//
//  def apply(
//             condExpr: Expr,
//             thenBody: Statement,
//             elseifStmts: StatementBlock) = new IfStatement(condExpr, thenBody,Some(elseifStmts),null)

  def apply(
             condExpr: Expr,
             thenBody: Statement,
             elseifStmts: Option[List[ElseifStatement]] = None,
             elseBody: Option[Statement] = None) = new IfStatement(condExpr, thenBody,elseifStmts, elseBody)

  def unapply(s: IfStatement) = Some(s.condExpr, s.thenBody,s.elseifBodies, s.elseBody)
}

class IfStatement(
                   private var _condExpr: Expr,
                   private var _thenBody: Statement,
                   private var _elseifBodies : Option[List[ElseifStatement]],
                   private var _elseBody: Option[Statement]) extends Statement{

  update(_condExpr, _thenBody,_elseifBodies, _elseBody)

  def condExpr = _condExpr
  def thenBody = _thenBody
  def elseifBodies  = _elseifBodies
  def elseBody = _elseBody

  def update(condExpr: Expr = _condExpr, thenBody: Statement = _thenBody, elseifBodies:Option[List[ElseifStatement]]= _elseifBodies, elseBody: Option[Statement] = _elseBody) = {
    _condExpr = condExpr
    _thenBody = thenBody
    _elseifBodies = elseifBodies
    _elseBody = elseBody
    thenBody.setParent(this)
    if (elseifBodies!= null&&elseifBodies != None) elseifBodies.get.foreach(elseifBody=>elseifBody.setParent(this))
    if (elseBody!= null&&elseBody != None) elseBody.get.setParent(this)
    this
  }

  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = IfStatement(
      condExpr.cloneExpr,
      thenBody.cloneStmt,
      elseifBodies ,
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
    str.append(String.format("if %s\n", condExpr.pretty()))
    str.append(thenBody.pretty(level + 1))
    if(elseifBodies!=null&& elseifBodies.get.size!=0&&elseifBodies.isDefined){

      elseifBodies.get.foreach(eachElseif =>
        str.append(eachElseif.pretty(level+1))
      )

     // str.append(elseifBodies.get.foreach(eachElseif=>eachElseif.pretty(level+1)))
    }
    if (elseBody!=null&&elseBody.isDefined) {
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

    if(elseifBodies!= null && elseifBodies.get.size!=0 && elseifBodies.isDefined){
      str.append(indentStr(level))
      elseifBodies.get.foreach(each => {
        if(each!=null)
        str.append(each.treePretty(level+2))
      })
      //str.append(elseifBodies.get.foreach(eachElseif=>eachElseif.pretty(level+2)))
    }

    if (elseBody!=null&&elseBody.isDefined) {
      str.append(indentStr(level))
      str.append("->ElseBody:\n")

      str.append(elseBody.get.treePretty(level+2))
    }
    str.toString
  }
}