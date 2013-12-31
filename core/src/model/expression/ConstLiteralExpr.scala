package model.expression

import   model._
import   model.expression._
import core._

object ConstLiteralImplicits {
  implicit def str2ConstLiteral(value: String)= ConstLiteralExpr(value)
  implicit def bool2ConstLiteral(value: Boolean)= ConstLiteralExpr(value)
  
  implicit def char2ConstLiteral(value: Char) = ConstLiteralExpr(value)
  implicit def byte2ConstLiteral(value: Byte) = ConstLiteralExpr(value)
  implicit def short2ConstLiteral(value: Short) = ConstLiteralExpr(value)
  implicit def int2ConstLiteral(value: Int) = ConstLiteralExpr(value)
  implicit def long2ConstLiteral(value: Long) = ConstLiteralExpr(value)
  
  implicit def float2ConstLiteral(value: Float) = ConstLiteralExpr(value)
  implicit def double2ConstLiteral(value: Double) = ConstLiteralExpr(value)
  
  implicit def cf2ConstLiteral(value: ComplexFloat) = ConstLiteralExpr(value)
  implicit def cd2ConstLiteral(value: ComplexDouble) = ConstLiteralExpr(value)
  
  implicit def num2ConstLiteral(value: NumericValue) = ConstLiteralExpr(value)
}

object ConstLiteralExpr {
  def apply(value: String) = new ConstLiteralExpr(StringType(), value)
  def apply(value: Boolean) = new ConstLiteralExpr(BooleanType(), value)

  def apply(value: Char) = new ConstLiteralExpr(CharType(), value)
  def apply(value: Byte) = new ConstLiteralExpr(ByteType(), value)
  def apply(value: Short) = new ConstLiteralExpr(ShortType(), value)
  def apply(value: Int) = new ConstLiteralExpr(IntType(), value)
  def apply(value: Long) = new ConstLiteralExpr(LongType(), value)
  
  def apply(value: Float) = new ConstLiteralExpr(FloatType(), value)
  def apply(value: Double) = new ConstLiteralExpr(DoubleType(), value)
  
  def apply(value: ComplexFloat) = new ConstLiteralExpr(ComplexFloatType(), value)
  def apply(value: ComplexDouble) = new ConstLiteralExpr(ComplexDoubleType(), value)
  
  def apply(value: NumericValue) = new ConstLiteralExpr(TypeUtil.anyType(value.value), value.value)
  
  def unapply(e: ConstLiteralExpr): Option[BasicType] = Some(e.kind)
}

class ConstLiteralExpr(val kind: BasicType, val value: Any) extends Expr {
  def castValue[T]: T = value.asInstanceOf[T]
  
  def numeric: NumericValue = NumericValue(value)
  
  def cloneExpr(): Expr = {
    val c = new ConstLiteralExpr(kind, value)
    c.base_copyFrom(this)
    c
  }
  
  def isString(): Boolean = kind.isInstanceOf[StringType]
  def isBoolean(): Boolean = kind.isInstanceOf[BooleanType]
  def isNumeric: Boolean = kind.isInstanceOf[NumericType]
  override def isLeafNode(): Boolean = true

  override def equals(that: Any): Boolean = that match {
    case o: ConstLiteralExpr => this.kind == o.kind && this.value == o.value
    case o: Boolean => isBoolean && castValue[Boolean] == o
    case o: String => isString && castValue[String] == o
    
    case x: Char          => isNumeric && numeric == x
    case x: Byte          => isNumeric && numeric == x
    case x: Short         => isNumeric && numeric == x
    case x: Int           => isNumeric && numeric == x
    case x: Long          => isNumeric && numeric == x
    case x: Float         => isNumeric && numeric == x
    case x: Double        => isNumeric && numeric == x
    case x: ComplexFloat  => isNumeric && numeric == x
    case x: ComplexDouble => isNumeric && numeric == x
    
    case _ => false
  }
  
  override def pretty(hash: Boolean = false): String = value.toString
  
  override def treePretty(level: Int = 0, hash: Boolean = false): String = {
    indentStr(level) + "ConstLiteralExpr: " + value.toString + "\n"
  }
}

