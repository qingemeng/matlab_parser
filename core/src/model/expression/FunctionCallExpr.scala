package model.expression


import   model._
import   model.expression._

object FunctionCallExpr {
  def apply(funcNameExpr: Expr, params: List[Expr]) = new FunctionCallExpr(funcNameExpr, params)
  def unapply(e: FunctionCallExpr) = Some(e.funcNameExpr, e.params)
}

class FunctionCallExpr(private var _funcNameExpr: Expr, private var _params: List[Expr]) extends Expr {

  update(_funcNameExpr, _params)
  
  def funcNameExpr = _funcNameExpr
  def params = _params
  
  def update(funcNameExpr: Expr = _funcNameExpr, params: List[Expr] = _params) = {
    _funcNameExpr = funcNameExpr
    _params = params
    funcNameExpr.setParent(this)
    this
  }
  
  def cloneExpr(): Expr = {
    val c = new FunctionCallExpr(funcNameExpr.cloneExpr, params.map(e => e.cloneExpr))
    c.base_copyFrom(this)
    c
  }
  
  override def equals(that: Any): Boolean = that match {
    case o: FunctionCallExpr => this.funcNameExpr.equals(o.funcNameExpr) && this.params.corresponds(o.params)((t1, t2) => t1.equals(t2))
    case _ => false
  }
  
  override def pretty(hash: Boolean = false): String = {
    funcNameExpr.pretty(hash) + "(" + params.map(_.pretty(hash)).mkString(", ") + ")"
  }
  
  def treePretty(level: Int = 0, hash: Boolean = false): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    
    str.append("FunctionCallExpr: ")
    str.append(pretty(hash))
    str.append("\n")
    
    str.append(indentStr(level))
    str.append("->FunctionName:\n")
    str.append(funcNameExpr.treePretty(level+2, hash))
    
    str.append(indentStr(level))
    str.append("->Parameters:\n")
    params.foreach(t => {
      str.append(t.treePretty(level+2))
    })
    
    str.toString
  }
}