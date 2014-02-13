package   model

import   model.expression.Expr
import   model.expression.IdExpr

trait BaseOp {
  def pretty(): String
  def toString(): String
}
trait UnaryOp extends BaseOp {
  def pretty(pettyTerm: String): String
  override def pretty() = pretty("")
}
trait NAryOp extends BaseOp
trait AssignOp extends BaseOp
trait ReductionOp

//----------- Assignment operators -----------------
case class OpAssign() extends AssignOp {
  override def pretty() = " = "
  override def toString() = "Assign"
}
case class OpPlusAssign() extends AssignOp with ReductionOp {
  override def pretty() = " += "
  override def toString() = "PlusAssign"
}
case class OpMinusAssign() extends AssignOp with ReductionOp {
  override def pretty() = " -= "
  override def toString() = "MinusAssign"
}
case class OpTimesAssign() extends AssignOp with ReductionOp {
  override def pretty() = " *= "
  override def toString() = "TimesAssign"
}
case class OpDivideAssign() extends AssignOp with ReductionOp {
  override def pretty() = " /= "
  override def toString() = "DivideAssign"
}
case class OpPowAssign() extends AssignOp with ReductionOp {
  override def pretty() = " <^>= "
  override def toString() = "PowAssign"
}

case class OpModuloAssign() extends AssignOp with ReductionOp {
  override def pretty() = " %= "
  override def toString() = "ModuloAssign"
}
case class OpShiftLeftAssign() extends AssignOp with ReductionOp {
  override def pretty() = " <<= "
  override def toString() = "ShiftLeftAssign"
}
case class OpShiftRightAssign() extends AssignOp with ReductionOp {
  override def pretty() = " >>= "
  override def toString() = "ShiftRightAssign"
}
case class OpBinaryAndAssign() extends AssignOp with ReductionOp {
  override def pretty() = " &= "
  override def toString() = "BinaryAndAssign"
}
case class OpBinaryXorAssign() extends AssignOp with ReductionOp {
  override def pretty() = " ^= "
  override def toString() = "BinaryXorAssign"
}
case class OpBinaryOrAssign() extends AssignOp with ReductionOp {
  override def pretty() = " |= "
  override def toString() = "BinaryOrAssign"
}
case class OpLogicalAndAssign() extends AssignOp with ReductionOp {
  override def pretty() = " &&= "
  override def toString() = "LogicalAndAssign"
}
case class OpLogicalOrAssign() extends AssignOp with ReductionOp {
  override def pretty() = " ||= "
  override def toString() = "LogicalOrAssign"
}


//----------- NAry operators -----------------
case class OpPlus() extends NAryOp {
  override def pretty() = " + "
  override def toString() = "Plus"
}
case class OpTimes() extends NAryOp {
  override def pretty() = " * "
  override def toString() = "MatTimes"
}
case class OpPow() extends NAryOp {
  override def pretty() = " ^ "
  override def toString() = "MatPower"
}

// Special operators
case class OpDotProd() extends NAryOp {
  override def pretty() = " .* "
  override def toString() = "ArrayTimes"
}
//case class OpOuterProd() extends NAryOp {
//  override def pretty() = " <o> "
//  override def toString() = "OuterProd"
//}
//case class OpMatProd() extends NAryOp {
//  override def pretty() = " ** "
//  override def toString() = "MatProd"
//}
case class OpMatPow() extends NAryOp {
  override def pretty() = " *^ "
  override def toString() = "ArrayPow"
}

// Extracted from IASTBinaryExpression
// Binary only
case class OpModulo() extends NAryOp {
  override def pretty() = " % "
  override def toString() = "Modulo"
}
//case class OpShiftLeft() extends NAryOp {
//  override def pretty() = " << "
//  override def toString() = "ShiftLeft"
//}
//case class OpShiftRight() extends NAryOp {
//  override def pretty() = " >> "
//  override def toString() = "ShiftRight"
//}
case class OpLessThan() extends NAryOp {
  override def pretty() = " < "
  override def toString() = "LessThan"
}
case class OpLessEq() extends NAryOp {
  override def pretty() = " <= "
  override def toString() = "LessEq"
}
case class OpGreaterThan() extends NAryOp {
  override def pretty() = " > "
  override def toString() = "GreaterThan"
}
case class OpGreaterEq() extends NAryOp {
  override def pretty() = " >= "
  override def toString() = "GreaterEq"
}
case class OpEquals() extends NAryOp {
  override def pretty() = " == "
  override def toString() = "Equals"
}
case class OpNotEquals() extends NAryOp {
  override def pretty() = " != "
  override def toString() = "NotEquals"
}
// Binary and above
case class OpBinaryAnd() extends NAryOp {
  override def pretty() = " & "
  override def toString() = "BinaryAnd"
}
case class OpBinaryXor() extends NAryOp {
  override def pretty() = " ^ "
  override def toString() = "BinaryXor"
}
case class OpBinaryOr() extends NAryOp {
  override def pretty() = " | "
  override def toString() = "BinaryOr"
}
case class OpLogicalAnd() extends NAryOp {
  override def pretty() = " && "
  override def toString() = "LogicalAnd"
}
case class OpLogicalOr() extends NAryOp {
  override def pretty() = " || "
  override def toString() = "LogicalOr"
}


// OpMinus and OpDivide should be converted to OpNegate and OpInvert before DSL refactoring
case class OpMinus() extends NAryOp {
  override def pretty() = " - "
  override def toString() = "Minus"
}
case class OpDivide() extends NAryOp {
  override def pretty() = " / "
  override def toString() = "Divide"
}
// The operators below are removed during AssignmentStatementPass
case class OpTempAssign() extends NAryOp {
  override def pretty() = " = "
  override def toString() = "TempAssign"
}
case class OpTempPlusAssign() extends NAryOp {
  override def pretty() = " += "
  override def toString() = "TempPlusAssign"
}
case class OpTempMinusAssign() extends NAryOp {
  override def pretty() = " -= "
  override def toString() = "TempMinusAssign"
}
case class OpTempTimesAssign() extends NAryOp {
  override def pretty() = " *= "
  override def toString() = "TempTimesAssign"
}
case class OpTempDivideAssign() extends NAryOp {
  override def pretty() = " /= "
  override def toString() = "TempDivideAssign"
}
case class OpTempModuloAssign() extends NAryOp {
  override def pretty() = " %= "
  override def toString() = "TempModuloAssign"
}
case class OpTempShiftLeftAssign() extends NAryOp {
  override def pretty() = " <<= "
  override def toString() = "TempShiftLeftAssign"
}
case class OpTempShiftRightAssign() extends NAryOp {
  override def pretty() = " >>= "
  override def toString() = "TempShiftRightAssign"
}
case class OpTempBinaryAndAssign() extends NAryOp {
  override def pretty() = " &= "
  override def toString() = "TempBinaryAndAssign"
}
case class OpTempBinaryXorAssign() extends NAryOp {
  override def pretty() = " ^= "
  override def toString() = "TempBinaryXorAssign"
}
case class OpTempBinaryOrAssign() extends NAryOp {
  override def pretty() = " |= "
  override def toString() = "TempBinaryOrAssign"
}

case class OpMin() extends NAryOp {
  override def pretty() = "min"
  override def toString() = "Minimum"
}
case class OpMax() extends NAryOp {
  override def pretty() = "max"
  override def toString() = "Maximum"
}


//----------- Unary operators -----------------
case class OpNegate() extends UnaryOp {
  override def pretty(prettyTerm: String) = "-" + prettyTerm
  override def toString() = "Negate"
}
case class OpInvert() extends UnaryOp {
  override def pretty(prettyTerm: String) = "1/" + prettyTerm
  override def toString() = "Invert"
}
case class OpTranspose() extends UnaryOp {
  override def pretty(prettyTerm: String) = "Transpose(" + prettyTerm + ")"
  override def toString() = "Transpose"
}

// Special operators
case class OpCast(targetType: BasicType) extends UnaryOp {
  override def pretty(prettyTerm: String) = "Cast(" + targetType + ", " + prettyTerm + ")"
  override def toString() = "Cast"
}

//Empty dims => sum of all dimensions
case class OpSum(dims: List[Int] = List.empty[Int]) extends UnaryOp {
  override def pretty(prettyTerm: String) = "Sum(" + prettyTerm + ", " + dims.mkString("{", " ", "}") + ")"
  override def toString() = "Sum"
}
case class OpProd(dims: List[Int]) extends UnaryOp {
  override def pretty(prettyTerm: String) = "Product(" + prettyTerm + ", " + dims.mkString("{", " ", "}") + ")"
  override def toString() = "Product"
}
// Order of slice increases from left to right
case class OpPermute(order: List[Int]) extends UnaryOp	 {
  override def pretty(prettyTerm: String) = "Permute(" + prettyTerm + ", " + order.mkString("{", " ", "}") + ")"
  override def toString() = "Permute"
}

// Extracted from IASTUnaryExpression
case class OpPrefixInc() extends UnaryOp {
  override def pretty(prettyTerm: String) = "++" + prettyTerm
  override def toString() = "PrefixInc"
}
case class OpPrefixDec() extends UnaryOp {
  override def pretty(prettyTerm: String) = "--" + prettyTerm
  override def toString() = "PrefixDec"
}
case class OpPostfixInc() extends UnaryOp {
  override def pretty(prettyTerm: String) = prettyTerm + "++"
  override def toString() = "PostfixInc"
}
case class OpPostfixDec() extends UnaryOp {
  override def pretty(prettyTerm: String) = prettyTerm + "--"
  override def toString() = "PostfixDec"
}
case class OpStar() extends UnaryOp {
  override def pretty(prettyTerm: String) = "*" + prettyTerm
  override def toString() = "Star"
}
case class OpAmpersand() extends UnaryOp {
  override def pretty(prettyTerm: String) = "&" + prettyTerm
  override def toString() = "Ampersand"
}
case class OpTilde() extends UnaryOp {
  override def pretty(prettyTerm: String) = "~" + prettyTerm
  override def toString() = "Tilde"
}
case class OpNot() extends UnaryOp {
  override def pretty(prettyTerm: String) = "!" + prettyTerm
  override def toString() = "Not"
}
case class OpSizeof() extends UnaryOp {
  override def pretty(prettyTerm: String) = "Sizeof(" + prettyTerm + ")"
  override def toString() = "Sizeof"
}

//----------- Misc functions -----------------
case class OpAbs() extends UnaryOp {
  override def pretty(prettyTerm: String) = "abs(" + prettyTerm + ")"
  override def toString() = "Absolute"
}
case class OpSqrt() extends UnaryOp {
  override def pretty(prettyTerm: String) = "sqrt(" + prettyTerm + ")"
  override def toString() = "SquareRoot"
}
case class OpLog() extends UnaryOp {
  override def pretty(prettyTerm: String) = "log(" + prettyTerm + ")"
  override def toString() = "Logarithm"
}
case class OpExp() extends UnaryOp {
  override def pretty(prettyTerm: String) = "exp(" + prettyTerm + ")"
  override def toString() = "Exponent"
}
case class OpSin() extends UnaryOp {
  override def pretty(prettyTerm: String) = "sin(" + prettyTerm + ")"
  override def toString() = "Sine"
}
case class OpCos() extends UnaryOp {
  override def pretty(prettyTerm: String) = "cos(" + prettyTerm + ")"
  override def toString() = "Cosine"
}
case class OpTan() extends UnaryOp {
  override def pretty(prettyTerm: String) = "tan(" + prettyTerm + ")"
  override def toString() = "Tangent"
}
case class OpASin() extends UnaryOp {
  override def pretty(prettyTerm: String) = "asin(" + prettyTerm + ")"
  override def toString() = "ArchSine"
}
case class OpACos() extends UnaryOp {
  override def pretty(prettyTerm: String) = "acos(" + prettyTerm + ")"
  override def toString() = "ArchCosine"
}

case class OpCeil() extends UnaryOp {
  override def pretty(prettyTerm: String) = "ceil(" + prettyTerm + ")"
  override def toString() = "Ceil"
}
case class OpFloor() extends UnaryOp {
  override def pretty(prettyTerm: String) = "floor(" + prettyTerm + ")"
  override def toString() = "Floor"
}

//----------- Complex functions -----------------
case class OpReal() extends UnaryOp {
  override def pretty(prettyTerm: String) = "real(" + prettyTerm + ")"
  override def toString() = "Real"
}
case class OpImag() extends UnaryOp {
  override def pretty(prettyTerm: String) = "imag(" + prettyTerm + ")"
  override def toString() = "Imaginary"
}
case class OpConj() extends UnaryOp {
  override def pretty(prettyTerm: String) = "conj(" + prettyTerm + ")"
  override def toString() = "Conjugate"
}

//----------- Utils functions -----------------
object OpUtils {
  def isTempAssignOp(op: NAryOp): Boolean = op match {
    case OpTempAssign()           => true 
    case OpTempPlusAssign()       => true
    case OpTempMinusAssign()      => true
    case OpTempTimesAssign()      => true
    case OpTempDivideAssign()     => true
    case OpTempModuloAssign()     => true
    case OpTempShiftLeftAssign()  => true
    case OpTempShiftRightAssign() => true
    case OpTempBinaryAndAssign()  => true
    case OpTempBinaryXorAssign()  => true
    case OpTempBinaryOrAssign()   => true
    case _                        => false
  }
  
  def isIncOrDec(op: UnaryOp): Boolean = op match {
    case OpPrefixInc()  => true
    case OpPrefixDec()  => true
    case OpPostfixInc() => true
    case OpPostfixDec() => true
    case _              => false
  }
  
  def isReduction(op: AssignOp) = op.isInstanceOf[ReductionOp]
}