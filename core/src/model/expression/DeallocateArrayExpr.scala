package model.expression


import   model._
import   model.expression._

object DeallocateArrayExpr {
  def apply(idName: IdName) = new DeallocateArrayExpr(idName)
  def unapply(e: DeallocateArrayExpr) = Some(e.idName)
}

class DeallocateArrayExpr(val idName: IdName) extends Expr {
  def cloneExpr(): Expr = {
    val c = new DeallocateArrayExpr(idName.cloneName)
    c.base_copyFrom(this)
    c
  }
  
  override def equals(that: Any): Boolean = that match {
    case o: DeallocateArrayExpr => this.idName.equals(o.idName)
    case _ => false
  }
  
  override def pretty(hash: Boolean = false): String = {
    "delete " + idName.pretty(hash)
  }
  
  override def treePretty(level: Int = 0, hash: Boolean = false): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    
    str.append("DeallocateArrayExpr: ")
    str.append(pretty(hash))
    str.append("\n")
    
    str.append(idName.treePretty(level+1, hash))
        
    str.toString
  }
}