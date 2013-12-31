package model


import model.expression._
import core._

object TypeUtil {
  val CHAR = 0
  val BYTE = 1
  val SHORT = 2
  val INT = 3
  val LONG = 4
  val FLOAT = 5
  val DOUBLE = 6
  val COMPLEX_FLOAT = 7
  val COMPLEX_DOUBLE = 8
  val OTHER = 9

  def isNumeric(v: Any) = v match {
    case x: Char => true
    case x: Byte => true
    case x: Short => true
    case x: Int => true
    case x: Long => true
    case x: Float => true
    case x: Double => true
    case x: ComplexFloat => true
    case x: ComplexDouble => true
    case _ => false
  }

  def anyCode(v: Any) = v match {
    case x: Char => CHAR
    case x: Byte => BYTE
    case x: Short => SHORT
    case x: Int => INT
    case x: Long => LONG
    case x: Float => FLOAT
    case x: Double => DOUBLE
    case x: ComplexFloat => COMPLEX_FLOAT
    case x: ComplexDouble => COMPLEX_DOUBLE
    case _ => OTHER
  }

  def anyType(v: Any) = codeType(anyCode(v))

  def typeCode(t: BasicType) = t match {
    case x: CharType => CHAR
    case x: ByteType => BYTE
    case x: ShortType => SHORT
    case x: IntType => INT
    case x: LongType => LONG
    case x: FloatType => FLOAT
    case x: DoubleType => DOUBLE
    case x: ComplexFloatType => COMPLEX_FLOAT
    case x: ComplexDoubleType => COMPLEX_DOUBLE
    case _ => OTHER
  }

  def codeType(code: Int): BasicType = code match {
    case CHAR => CharType()
    case BYTE => ByteType()
    case SHORT => ShortType()
    case INT => IntType()
    case LONG => LongType()
    case FLOAT => FloatType()
    case DOUBLE => DoubleType()
    case COMPLEX_FLOAT => ComplexFloatType()
    case COMPLEX_DOUBLE => ComplexDoubleType()
    case OTHER => UnsupportedType()
  }

  def maxCode(v1: Any, v2: Any) = {
    val code1 = anyCode(v1)
    val code2 = anyCode(v2)
    scala.math.max(code1, code2)
  }

  def maxType(t1: BasicType, t2: BasicType):BasicType = {
    val code1 = typeCode(t1)
    val code2 = typeCode(t2)
    val maxCode = scala.math.max(code1, code2)
    codeType(maxCode)
  }

  //Calculate datatype of model.expression
  def getDataType(declUseMap: DeclUseMap, term: Expr): Int = term match {
    case UnaryExpr(op, term) => op match {
      case OpCast(targetType) => typeCode(targetType)
      case _ => getDataType(declUseMap, term)
    }

    case NAryExpr(op, terms) => terms.map(term => getDataType(declUseMap, term)).max

    case IdExpr(idName) => {
      val declUseGroup = declUseMap.getDeclUse(idName)
      typeCode(declUseGroup.getDeclarationType)
    }
    case ConstLiteralExpr(kind) => typeCode(kind)

    case ArrayRefExpr(id, _) => {
      val idName = id match {
        case IdExpr(name) => name
        case _ => throw new UnsupportedOperationException("Unknown array model.expression " + id.pretty())
      }

      val declUseGroup = declUseMap.getDeclUse(idName)
      val arrayInfo = declUseGroup.getArrayInfo
      typeCode(arrayInfo.arrayType)
    }

    //FIXME Use the data type of the first parameter 
    case FunctionCallExpr(name, params) => getDataType(declUseMap, params(0))

    case _ => -1
  }
}