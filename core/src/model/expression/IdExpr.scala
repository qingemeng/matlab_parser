package model.expression


import   model._
import   model.expression._

object IdExpr {
  def apply(idName: IdName) = new IdExpr(idName)
  def unapply(e: IdExpr) = Some(e.idName)
}

class IdExpr(val idName: IdName) extends Expr {

  def cloneExpr(): Expr = {
    val c = new IdExpr(idName.cloneName)
    c.base_copyFrom(this)
    c
  }
  
  override def isLValue(): Boolean = true
  override def isLeafNode(): Boolean = true
  
  override def equals(that: Any): Boolean = that match {
    case o: IdExpr => this.idName.equals(o.idName)
    case _ => false
  }
  
  override def pretty(hash: Boolean = false): String = idName.pretty(hash)
  
  override def treePretty(level: Int = 0, hash: Boolean = false): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    
    str.append("IdExpr: ")
    str.append(pretty(hash))
    str.append("\n")
    
    str.append(idName.treePretty(level+1, hash))
    
    str.toString
  }
}