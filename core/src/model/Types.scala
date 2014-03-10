package   model

import   model.expression._

//TODO: types should not have modifiers 

abstract class BasicType

case class UnsupportedType() extends BasicType {
  override def toString() = "UnsupportedType"
}

/**************************************************************************************/
case class ArrayType(subType: BasicType, rank: Int, sizes: List[Expr]) extends BasicType {
  override def toString() = s"ArrayType($subType, $rank, ${sizes.map(s => s.pretty()).mkString(", ")})" 
}
// Array Types
//abstract class BaseArrayType(subType: BasicType, rank: Int) extends BasicType
//
//// Array Types (size is -1 if unknown)
//case class ArrayType(subType: BasicType, rank: Int) extends BaseArrayType(subType, rank)
//case class VectorType(subType: BasicType) extends BaseArrayType(subType, 1)
//case class MatrixType(subType: BasicType) extends BaseArrayType(subType, 2)

/**************************************************************************************/
// Basic Types
case class PointerType(subType: BasicType) extends BasicType {
  override def toString() = s"PointerType($subType)"
}

case class StringType() extends BasicType {
  override def toString() = "String"
}
case class BooleanType() extends BasicType {
  override def toString() = "Boolean"
}
case class NullType() extends BasicType{
  override def toString() = "Null"
}

sealed trait NumericType extends BasicType

// Integer Types
sealed trait IntegerType extends NumericType
case class CharType() extends IntegerType {
  override def toString() = "Char"
}
case class ByteType() extends IntegerType {
  override def toString() = "Byte"
}
case class ShortType() extends IntegerType {
  override def toString() = "Short"
}
case class IntType() extends IntegerType {
  override def toString() = "Int"
}
case class LongType() extends IntegerType {
  override def toString() = "Long"
}

// Floating Types
sealed trait FloatingType extends NumericType
case class FloatType() extends FloatingType {
  override def toString() = "Float"
}
case class DoubleType() extends FloatingType {
  override def toString() = "Double"
}

/**************************************************************************************/
// Complex Types

sealed trait ComplexType extends NumericType

case class ComplexFloatType() extends ComplexType {
  override def toString() = "ComplexFloat"
}
case class ComplexDoubleType() extends ComplexType {
  override def toString() = "ComplexDouble"
}