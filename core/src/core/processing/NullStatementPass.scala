package   core.processing

import   model._
import   model.statement._
import   model.expression._

// Removes null statements or statements that has no effects
// * Must be preceded by StatementBlockPass
// * Should succeed SideEffectPass
class NullStatementPass extends StatementVisitor {
  
  private def removeStatement(stmt: Statement) {
    if (!stmt.getParent.isInstanceOf[StatementBlock]) 
      throw new UnsupportedOperationException(stmt.getParent.toString)
    val blk = stmt.getParent.asInstanceOf[StatementBlock]
    blk.removeStatement(stmt)
  }
  
  override def visit(stmt: NullStatement): Int = {
    removeStatement(stmt)
    return StatementVisitor.Continue
  }
  
  override def visit(stmt: ExpressionStatement): Int = {
    if (NullStatementPass.isNullEffectExpr(stmt.expr)) removeStatement(stmt)
    return StatementVisitor.Continue
  }
}

object NullStatementPass {
  def doPass(tu: TranslationUnit): Unit = {
    StatementProcessor.process(tu, new NullStatementPass())
  }

  def doPass(function: FunctionDef): Unit = {
    StatementProcessor.process(function, new NullStatementPass())
  }
  
  // Returns true if expression has no effects or side effects
  def isNullEffectExpr(xpr: Expr): Boolean = xpr match {
    // TODO: need to check if function calls have side effects
    // currently assumes we cannot remove function calls
    case e: FunctionCallExpr    => false
    // TODO: conditional
    case e: ConditionalExpr     => false
    case e: ConstLiteralExpr    => true
    case e: IdExpr              => true
    case e: TypeIdExpr          => true
    case e: FieldRefExpr        => true
    case e: ArrayRefExpr        => isNullEffectExpr(e.owner)
    case e: UnaryExpr           => isNullEffectExpr(e.term)
    case e: NAryExpr            => !OpUtils.isTempAssignOp(e.op)
    case e: DeallocateArrayExpr => false
  }
}