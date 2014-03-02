package model.expression

import   model._
import   model.expression._

object ArrayRefExpr {
  def apply(owner: Expr, indices: List[Expr])  = new ArrayRefExpr(owner, indices)
  def unapply(e: ArrayRefExpr) = Some(e.owner, e.indices)
}

class ArrayRefExpr private(var _owner: Expr, var _indices: List[Expr]) extends Expr {
  update(_owner, _indices)
  
  def owner = _owner
  def indices = _indices
  
  def update(owner: Expr = _owner, indices: List[Expr] = _indices) = {
    _owner = owner
    _indices = indices
    owner.setParent(this)
    this  
  }
  
  override def cloneExpr(): ArrayRefExpr = {
    val c = new ArrayRefExpr(owner.cloneExpr, indices.map(index => index.cloneExpr))
    c.base_copyFrom(this)
    c
  }
  
  override def isLValue(): Boolean = true
  override def isLeafNode(): Boolean = true

  def rank = indices.length
  
  override def equals(that: Any): Boolean = that match {
    case o: ArrayRefExpr => 
      this.owner.equals(o.owner) && this.indices.corresponds(o.indices)((t1, t2) => t1.equals(t2))
    case _ => false
  }
  
  override def pretty(hash: Boolean = false): String = {
    owner.pretty(hash) + indices.map(i => "[" + i.pretty(hash) + "]").mkString
  }
  
  override def treePretty(level: Int = 0, hash: Boolean = false): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("ArrayRefExpr: ")
    str.append(pretty(hash))
    str.append("\n")        
 
    str.append(indentStr(level+1))
    str.append("Owner:\n")
    str.append(owner.treePretty(level+2, hash))
    
    str.append(indentStr(level))
    str.append("->Indices:\n")
    indices.foreach(i => str.append(i.treePretty(level+2, hash)))
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