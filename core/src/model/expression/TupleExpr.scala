package model.expression


import   model._
import   model.expression._

object TupleExpr {
  def apply(exprs: List[Expr]) = new TupleExpr(exprs)
  def unapply(e: TupleExpr) = Some(e.exprs)
}

class TupleExpr(private var _exprs: List[Expr]) extends Expr {

  update(_exprs)
  
  def exprs = _exprs
  
  def update(exprs: List[Expr] = _exprs) = {
    _exprs = exprs
    exprs.foreach(e => e.setParent(this))
    this
  }

  def cloneExpr(): Expr = {
    val c = new TupleExpr(exprs.map(e => e.cloneExpr))
    c.base_copyFrom(this)
    c
  }
  
  override def equals(that: Any): Boolean = that match {
    case o: TupleExpr => this.exprs.corresponds(o.exprs)((t1, t2) => t1.equals(t2))
    case _ => false
  }
  
  override def pretty(hash: Boolean = false): String = {
    "[" + exprs.map(t => t.pretty()).mkString(", ") + "]"
  }
  
  override def treePretty(level: Int = 0, hash: Boolean = false): String = {
    val str = new StringBuilder
    
    str.append(indentStr(level))
    str.append("TupleExpr: ")
    str.append(pretty(hash))
    str.append("\n")
    exprs.foreach(t => {
      str.append(t.treePretty(level+1))
      str.append("\n")
    })
    
    str.toString
  }
}