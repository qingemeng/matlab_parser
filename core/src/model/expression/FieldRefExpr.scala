package model.expression


import   model._
import   model.expression._

object FieldRefExpr {
  def apply(owner: Expr, fieldName: IdName, isPointerDereference: Boolean) =
    new FieldRefExpr(owner, fieldName, isPointerDereference)
  def unapply(e: FieldRefExpr) = Some(e.owner, e.fieldName, e.isPointerDereference)
}

class FieldRefExpr(
    private var _owner: Expr, 
    private var _fieldName: IdName, 
    private var _isPointerDereference: Boolean) extends Expr {

  update(_owner, _fieldName, _isPointerDereference)
  
  def owner = _owner
  def fieldName = _fieldName
  def isPointerDereference = _isPointerDereference
  
  def update(owner: Expr = _owner, fieldName: IdName = _fieldName, isPointerDereference: Boolean = _isPointerDereference) = {
    _owner = owner
    _fieldName = fieldName
    _isPointerDereference = isPointerDereference
    owner.setParent(this)
    this  
  }
  
  def cloneExpr(): Expr = {
    val c = new FieldRefExpr(owner.cloneExpr, fieldName.cloneName, isPointerDereference)
    c.base_copyFrom(this)
    c
  }
  
  override def isLValue(): Boolean = true

  override def equals(that: Any): Boolean = that match {
    case o: FieldRefExpr => this.owner.equals(o.owner) && this.fieldName.equals(o.fieldName) && this.isPointerDereference == o.isPointerDereference
    case _ => false
  }
  
  override def pretty(hash: Boolean = false): String = {
    owner.pretty(hash) + (if (isPointerDereference) "->" else ".") + fieldName.pretty(hash)
  }
  
  override def treePretty(level: Int = 0, hash: Boolean = false): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    
    str.append("FieldRefExpr: ")
    str.append(pretty(hash))
    str.append("\n")
    
    str.append(owner.treePretty(level+1, hash))
    
    str.toString
  }
  //TODO:gm,rewrite
  override def semanticAnalyse(level: Int = 0, hash: Boolean = false): String = {
    val str = new StringBuilder
    str.append(indentStr(level))

    str.append("FieldRefExpr: ")
//    str.append(pretty(hash))
    str.append("\n")

    str.append(owner.semanticAnalyse(level+1, hash))

    str.toString
  }
}