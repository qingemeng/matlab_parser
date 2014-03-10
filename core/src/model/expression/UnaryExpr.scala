package model.expression


import   model._
import   model.expression._


object UnaryExpr {
  def apply(op: UnaryOp, term: Expr) = new UnaryExpr(op, term)
  def unapply(e: UnaryExpr) = Some(e.op, e.term)
}

class UnaryExpr(private var _op: UnaryOp, private var _term: Expr) extends Expr {
  
  update(_op, _term)
  
  def op = _op
  def term = _term
  
  def update(op: UnaryOp = _op, term: Expr = _term) = {
    _op = op
    _term = term
    term.setParent(this)
    this
  }

  def cloneExpr(): Expr = {
    val c = new UnaryExpr(op, term.cloneExpr)
    c.base_copyFrom(this)
  }
  
  override def isLValue(): Boolean = {
    op match {
      case OpStar() => term.isLValue()
      case _        => false
    }
  }
  
  override def isLeafNode(): Boolean = term.isLeafNode()

  override def equals(that: Any): Boolean = that match {
    case o: UnaryExpr => this.op == o.op && this.term.equals(o.term)
    case _ => false
  }

  override def pretty(hash: Boolean = false): String = op.pretty(term.pretty(hash))
  
  override def treePretty(level: Int = 0, hash: Boolean = false): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("UnaryExpr(")
    str.append(op.toString())
    str.append("): ")
    
    str.append(pretty(hash))
    str.append("\n")
    str.append(term.treePretty(level+1))
    str.toString
  }
  //TODO:gm,rewrite
//  override def typePretty(level: Int = 0, hash: Boolean = false): String = {
//    val str = new StringBuilder
//    str.append(indentStr(level))
//    str.append("\n")
//    str.toString
//  }
}

