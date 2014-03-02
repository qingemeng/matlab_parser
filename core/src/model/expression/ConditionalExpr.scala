package model.expression

import   model._
import   model.expression._

object ConditionalExpr {
  def apply(condExpr: Expr, positiveExpr: Expr, negativeExpr: Expr) =
    new ConditionalExpr(condExpr, positiveExpr, negativeExpr)
  def unapply(e: ConditionalExpr) = Some(e.condExpr, e.positiveExpr, e.negativeExpr)
}

class ConditionalExpr(
    private var _condExpr: Expr, 
    private var _positiveExpr: Expr, 
    private var _negativeExpr: Expr) extends Expr {

  def condExpr = _condExpr
  def positiveExpr = _positiveExpr
  def negativeExpr = _negativeExpr
  
  def update(condExpr: Expr = _condExpr, positiveExpr: Expr = _positiveExpr, negativeExpr: Expr = _negativeExpr) = {
    _condExpr = condExpr
    _positiveExpr = positiveExpr
    _negativeExpr = negativeExpr
    condExpr.setParent(this)
    positiveExpr.setParent(this)
    negativeExpr.setParent(this)
    this
  }

  def cloneExpr(): Expr = {
    val c = new ConditionalExpr(condExpr.cloneExpr, positiveExpr.cloneExpr, negativeExpr.cloneExpr)
    c.base_copyFrom(this)
    c
  }
  
  override def equals(that: Any): Boolean = that match {
    case o: ConditionalExpr => this.condExpr.equals(o.condExpr) && this.positiveExpr.equals(o.positiveExpr) && this.negativeExpr.equals(o.negativeExpr)
    case _ => false
  }
  
  override def pretty(hash: Boolean = false): String = {
    condExpr.pretty(hash) + "?" + positiveExpr.pretty(hash) + ":" + negativeExpr.pretty(hash)
  }
  
  override def treePretty(level: Int = 0, hash: Boolean = false): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    
    str.append("ConditionalExpr: ")
    str.append(pretty(hash))
    str.append("\n")
    
    str.append(indentStr(level))
    str.append("->CondExpr:\n")
    str.append(condExpr.treePretty(level+2, hash))
    
    str.append(indentStr(level))
    str.append("->PositiveExpr:\n")
    str.append(positiveExpr.treePretty(level+2, hash))
    
    str.append(indentStr(level))
    str.append("->NegativeExpr:\n")
    str.append(negativeExpr.treePretty(level+2, hash))
    str.append("\n")
    
    str.toString
  }
  //TODO:gm,rewrite
  override def typePretty(level: Int = 0, hash: Boolean = false): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("\n")
    str.toString
  }
}