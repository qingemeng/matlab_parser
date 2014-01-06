//package model.statement
//
//import   model._
//import   model.statement._
//import   model.expression._
//import   model.property._
//
//object ForStatement2 {
//  def apply(
//             rangeExpr: Expr,
//             body: Statement) = new ForStatement2(rangeExpr, body)
//  def unapply(s: ForStatement2) = Some(s.rangeExpr, s.body)
//}
//
//// initStmt can be either DeclarationStatement or ExpressionStatement
//class ForStatement2(
//
//                    private var _rangeExpr: Expr,
//                    private var _body: Statement) extends Statement {
//
//  update(_rangeExpr, _body)
//
//  def rangeExpr = _rangeExpr
//  def body = _body
//
//  def update(rangeExpr: Expr = _rangeExpr, body: Statement = _body) = {
//    _rangeExpr = rangeExpr
//    _body = body
//    _body.setParent(this)
//    this
//  }
//
//  // deep clone of the model.statement
//  override def cloneStmt() = {
//    val newStmt = ForStatement2(rangeExpr.cloneExpr, body.cloneStmt)
//    newStmt.body.setParent(newStmt)
//    newStmt.base_copyFrom(this)
//    newStmt
//  }
//
//  override def pretty() = pretty(0)
//  override def pretty(level: Int): String = {
//
////    val rangeExprPretty = rangeExpr.map(_.pretty()).getOrElse("")
//    val str = new StringBuilder
//    //if (props[LoopProperty].getCanAbstractLoop) str.append("can_abstr>")
//    //else str.append("cannot_abstr>")
//    str.append(indentStr(level))
//    str.append(String.format("for %s\n", rangeExpr.pretty()))
//    str.append(body.pretty(level + 1))
//    str.toString
//  }
//
//  override def treePretty(level: Int = 0): String = {
//    val str = new StringBuilder
//    str.append(indentStr(level))
//    str.append("ForStatement: ")
//    //str.append(pretty())
//    str.append("\n")
//
//
////    if (rangeExpr.isDefined){
//      str.append(indentStr(level))
//      str.append("->rangeExpr: ")
//      str.append(rangeExpr.pretty())
////      str.append("\n")
////      str.append(rangeExpr.get.treePretty(level+2))
////    }
//
//    str.append(indentStr(level))
//    str.append("->ForBody:\n")
//    str.append(body.treePretty(level+2))
//
//    str.toString
//  }
//}
