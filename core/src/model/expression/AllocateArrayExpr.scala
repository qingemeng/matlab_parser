package model.expression

import   model._
import   model.expression._

object AllocateArrayExpr {
  def apply(arrayInfo: ArrayInfo) = new AllocateArrayExpr(arrayInfo)
  def unapply(e: AllocateArrayExpr) = Some(e.arrayInfo)
}

class AllocateArrayExpr(var arrayInfo: ArrayInfo) extends Expr {
  def cloneExpr(): Expr = {
    val c = new AllocateArrayExpr(arrayInfo)
    c.base_copyFrom(this)
  }
  
  def update(_arrayInfo: ArrayInfo) = {
    arrayInfo = _arrayInfo
    this
  }
  
  override def equals(that: Any): Boolean = that match {
    case o: AllocateArrayExpr => this.arrayInfo.equals(o.arrayInfo)
    case _ => false
  }
  
  override def pretty(hash: Boolean = false): String = {
    "new " + arrayInfo.pretty()
  }
  
  override def treePretty(level: Int = 0, hash: Boolean = false): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    
    str.append("AllocateArrayExpr: ")
    str.append(pretty(hash))
    str.append("\n")
    
    str.append(indentStr(level))
    str.append("->ArrayInfo:\n")
    str.append(arrayInfo.treePretty(level+2))
    str.toString
  }
}