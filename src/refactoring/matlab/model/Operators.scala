package refactoring.matlab.model

import   model._

case class OpLeftDivide() extends NAryOp {
  override def pretty() = " \\ "
  override def toString () = "LeftDivide"
}

case class OpLeftDivideAssign() extends AssignOp {
  override def pretty() = " \\= "
  override def toString () = "LeftDivideAssign"
}



case class OpUnaryPlus() extends UnaryOp {
  override def pretty(prettyTerm: String) = "+" + prettyTerm
  override def toString() = "UnaryPlus"
}
case class OpUnaryMinus() extends UnaryOp {
  override def pretty(prettyTerm: String) = "-" + prettyTerm
  override def toString() = "UnaryMinus"
}

case class EleWiseOpLogicalAnd() extends NAryOp {
  override def pretty() = " & "
  override def toString() = "ElementWiseLogicalAnd"
}
case class EleWiseOpLogicalOr() extends NAryOp {
  override def pretty() = " | "
  override def toString() = "ElementWiseLogicalOr"
}

case class OpMakeFactor() extends  UnaryOp{
  override def pretty(prettyTerm: String) =  prettyTerm
  override def toString() = "Make Array to a Array factor"
}

