package model.expression


import   model._
import   model.expression._

object TypeIdExpr {
  def apply(typeName: String) = new TypeIdExpr(typeName)
  def unapply(e: TypeIdExpr) = Some(e.typeName)
}

// Support e.g. sizeof(int)
class TypeIdExpr(val typeName: String) extends Expr {

  val op = "sizeof"
  
  def cloneExpr(): Expr = {
    val c = new TypeIdExpr(typeName)
    c.base_copyFrom(this)
    c
  }
 
  override def equals(that: Any): Boolean = that match {
    case o: TypeIdExpr => this.typeName == o.typeName
    case _ => false
  }
  
  override def pretty(hash: Boolean = false): String = {
    op + "(" + typeName  + ")"
  }
  
  override def treePretty(level: Int = 0, hash: Boolean = false): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    
    str.append("TypeIdExpr: ")
    str.append(pretty(hash))
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