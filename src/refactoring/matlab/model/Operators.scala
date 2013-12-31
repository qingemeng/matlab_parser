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

