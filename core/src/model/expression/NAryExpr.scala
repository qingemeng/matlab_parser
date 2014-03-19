package model.expression


import   model._
import   model.expression._

object NAryExpr {
  def apply(op: NAryOp, terms: List[Expr]) = new NAryExpr(op, terms)
  def unapply(e: NAryExpr) = Some(e.op, e.terms)
}

class NAryExpr(private var _op: NAryOp, private var _terms: List[Expr]) extends Expr {

  update(_op, _terms)

  def op = _op
  def terms = _terms
  
  def update(op: NAryOp = _op, terms: List[Expr] = _terms) = {
    _op = op
    _terms = terms
    terms.foreach(t => t.setParent(this));
    this
  }

  def cloneExpr(): Expr = {
    val c = new NAryExpr(op, terms.map(e => e.cloneExpr))
    c.base_copyFrom(this)
  }
  
  override def equals(that: Any): Boolean = that match {
    case o: NAryExpr => this.op == o.op && this.terms.corresponds(o.terms)((t1, t2) => t1.equals(t2))
    case _ => false
  }

  override def pretty(hash: Boolean = false): String = {
    terms.map(t => t.pretty(hash)).mkString("(", op.pretty(), ")")
  }
  
  override def treePretty(level: Int = 0, hash: Boolean = false): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("NAryExpr(")
    str.append(op.toString())
    str.append("): ")
    
    str.append(pretty(hash))
    str.append("\n")
    terms.foreach(term => {
      str.append(term.treePretty(level+1))
    })
    str.toString
  }
  //TODO:gm,rewrite
  override def semanticAnalyse(level: Int = 0, hash: Boolean = false): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("NAryExpr(")
    str.append(op.toString())
    str.append("): ")

//    str.append(pretty(hash))
    str.append("\n")
    terms.foreach(term => {
      str.append(term.semanticAnalyse(level+1))
    })
    str.toString
  }
}

