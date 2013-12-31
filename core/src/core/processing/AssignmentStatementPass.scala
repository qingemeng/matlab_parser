package   core.processing

import   model._
import   model.statement._
import   model.expression._

// Converts expression statement to assignment statement if possible
// Handles cases such as x=y=z
// Assumes parent is a StatementBlock
// * Must be preceded by StatementBlockPass
class AssignmentStatementPass extends StatementVisitor {
  
  override def visit(stmt: ExpressionStatement): Int = {
    var assignStmts = List.empty[AssignmentStatement]

    def processExpr(expr: Expr): Expr = {
      expr match {
        case e: NAryExpr =>
          // Note that lhs must be lvalue
          // although we do not verify here
          val lhs = e.terms(0)
          val rhs = processExpr(e.terms(1))
          def doit(op: AssignOp): Boolean = { assignStmts ::= AssignmentStatement(lhs, rhs, op); true }
          val processed = e.op match {
            case OpTempAssign()           => doit(OpAssign()) 
            case OpTempPlusAssign()       => doit(OpPlusAssign())
            case OpTempMinusAssign()      => doit(OpMinusAssign())
            case OpTempTimesAssign()      => doit(OpTimesAssign())
            case OpTempDivideAssign()     => doit(OpDivideAssign())
            case OpTempModuloAssign()     => doit(OpModuloAssign())
            case OpTempShiftLeftAssign()  => doit(OpShiftLeftAssign())
            case OpTempShiftRightAssign() => doit(OpShiftRightAssign())
            case OpTempBinaryAndAssign()  => doit(OpBinaryAndAssign())
            case OpTempBinaryXorAssign()  => doit(OpBinaryXorAssign())
            case OpTempBinaryOrAssign()   => doit(OpBinaryOrAssign())
            case _ => false
          }
          if (processed) {
            lhs
          } else {
            expr
          }
        case other => expr
      }
    }
    
    processExpr(stmt.expr)
    assignStmts = assignStmts.reverse
    
    if (!assignStmts.isEmpty) {
      if (!stmt.getParent.isInstanceOf[StatementBlock]) 
        throw new UnsupportedOperationException(stmt.getParent.toString)
      val blk = stmt.getParent.asInstanceOf[StatementBlock]

      for (s <- assignStmts) {
        blk.insertBefore(stmt, s)
      }
      blk.removeStatement(stmt)
    }
    StatementVisitor.Continue
  }
  
}

object AssignmentStatementPass {
  def doPass(tu: TranslationUnit): Unit = {
    StatementProcessor.process(tu, new AssignmentStatementPass())
  }
  def doPass(function: FunctionDef): Unit = {
    StatementProcessor.process(function, new AssignmentStatementPass())
  }
}