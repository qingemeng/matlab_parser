package model.expression

import core._
import   model._
import   model.expression._

object SliceExpr {
  def apply() = new SliceExpr(ConstLiteralExpr(0), ConstLiteralExpr(0), ConstLiteralExpr(0))
  
  //Default stride 1
  def apply(lowerBound: Expr, 
		    upperBound: Expr) = new SliceExpr(lowerBound, upperBound, ConstLiteralExpr(1))
}

// design decision: LinSpaceExpr is 1D not ND.
case class SliceExpr (private var _lowerBound: Expr, private var _upperBound: Expr, private var _stride: Expr) extends Expr {
  update(_lowerBound, _upperBound, _stride)
  
  def lowerBound = _lowerBound
  def upperBound = _upperBound
  def stride = _stride
  def size = Algebra.simplify((upperBound - lowerBound)/stride)
  
  def update(lowerBound: Expr = _lowerBound, upperBound: Expr = _upperBound, stride: Expr = _stride) = {
    _lowerBound = lowerBound
    _upperBound = upperBound
    _stride = stride
    
    _lowerBound.setParent(this)
    _upperBound.setParent(this)
    _stride.setParent(this)
    
    this
  }
  
  def cloneExpr(): Expr = {
    val c = new SliceExpr(lowerBound.cloneExpr(), upperBound.cloneExpr(), stride.cloneExpr())
    c.base_copyFrom(this)
  }
  
  override def equals(that: Any): Boolean = that match {
    case o: SliceExpr => this.lowerBound.equals(o.lowerBound) && this.upperBound.equals(o.upperBound) && this.stride.equals(o.stride)
    case _ => false
  }
  
  override def pretty(hash: Boolean = false): String = {
    lowerBound.pretty(hash) + ":" + upperBound.pretty(hash) + ":" +  stride.pretty(hash)
  }
  
  override def treePretty(level: Int = 0, hash: Boolean = false): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    
    str.append("SliceExpr: ")
    str.append(pretty(hash))
    str.append("\n")
    
    str.append(indentStr(level))
    str.append("->LowerBound: ")
    str.append(lowerBound.treePretty(level+2, hash))
    str.append("\n")
    
    str.append(indentStr(level))
    str.append("->UpperBound: ")
    str.append(upperBound.treePretty(level+2, hash))
    str.append("\n")
    
    str.append(indentStr(level))
    str.append("->Stride: ")
    str.append(stride.treePretty(level+2, hash))
    str.append("\n")
    
    str.toString
  }
}