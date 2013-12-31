package model.statement


import   model._
import   model.statement._
import   model.expression._
import scala.collection.mutable.ListBuffer
import   model.property.DomainsProperty

object AssignmentStatement {
  def apply(lhs: Expr, rhs: Expr, assignOp: AssignOp) = new AssignmentStatement(lhs, rhs, assignOp)
  def unapply(s: AssignmentStatement) = Some(s.lhsExpr, s.rhsExpr, s.assignOp)
}

class AssignmentStatement(
    private var _lhsExpr: Expr, 
    private var _rhsExpr: Expr, 
    private var _assignOp: AssignOp) extends Statement {

  update(_lhsExpr, _rhsExpr, _assignOp)
  
  def lhsExpr = _lhsExpr
  def rhsExpr = _rhsExpr
  def assignOp = _assignOp
  
  private var _writeRef: Expr = null
  private var _readRefs: List[Expr] = List.empty

  def writeRef = _writeRef
  def readRefs = _readRefs
  
  def update(lhsExpr: Expr = _lhsExpr, rhsExpr: Expr = _rhsExpr, op: AssignOp = _assignOp) = {
    _lhsExpr = lhsExpr
    _rhsExpr = rhsExpr
    _assignOp = op
    
    //FIXME Commented out for the time being
    
    //updateRWRefs()
    //checkForCompoundAssignment()
    
    this
  }
  
  // deep clone of the model.statement
  override def cloneStmt() = {
    val newStmt = AssignmentStatement(lhsExpr.cloneExpr(), rhsExpr.cloneExpr(), assignOp)
    newStmt.base_copyFrom(this)
    newStmt
  }
  
  private def updateRWRefs() {
    _writeRef = _lhsExpr
    val visitor = new FindReadRefVisitor()
    ExpressionProcessor.process(_rhsExpr, visitor)
    assignOp match {
      case OpAssign() => _readRefs = visitor.getReadRefs
      case _ => _readRefs = _lhsExpr::visitor.getReadRefs 
    }
    
  }
  
  // TODO: convert to compound assignment e.g. x = x + ... -> x +=
  private def checkForCompoundAssignment() {
    if (readRefs.contains(lhsExpr)) assignOp match {
      case OpAssign() => throw new UnsupportedOperationException("COMPASSIGN: " + pretty())
      case _          => throw new UnsupportedOperationException("??")
    }
  }
  
  override def pretty(): String = {
    //(if (props[DomainsProperty].hasDomains) ">" else " ") +
    lhsExpr.pretty() + assignOp.pretty() + rhsExpr.pretty()
  }
  
  override def treePretty(level: Int = 0): String = {
    val str = new StringBuilder
    str.append(indentStr(level))
    str.append("AssignmentStatement(")
    str.append(assignOp.toString())
    str.append("): ")
    str.append(pretty())
    str.append("\n")
    
    str.append(indentStr(level))
    str.append("->LHS: ")
    str.append(lhsExpr.pretty())
    str.append("\n")
    str.append(lhsExpr.treePretty(level+2))

    str.append(indentStr(level))
    str.append("->RHS: ")
    str.append(rhsExpr.pretty())
    str.append("\n")
    str.append(rhsExpr.treePretty(level+2))
    
    str.toString
  }
}

private class FindReadRefVisitor extends ExpressionVisitor {
  
  private val _readRefs = ListBuffer.empty[Expr]
  
  def getReadRefs = _readRefs.toList

  override def leave(expr: IdExpr): Expr = {
    _readRefs.append(expr)
    expr
  }
  override def leave(expr: FieldRefExpr): Expr = {
    _readRefs.append(expr)
    expr
  }
  override def leave(expr: ArrayRefExpr): Expr = {
    _readRefs.append(expr)
    expr
  }
  
//  // go through NAryExpr to find whole terms
//  override def visit(expr: NAryExpr): Int = {
//    for (t <- expr.terms) t match {
//      case e: IdExpr       => _readRefs.append(e)
//      case e: FieldRefExpr => _readRefs.append(e)
//      case e: ArrayRefExpr => _readRefs.append(e)
//      case _ =>
//    }
//    return ExpressionVisitor.Continue
//  }
  
}