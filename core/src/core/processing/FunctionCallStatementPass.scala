package   core.processing

import   core._
import   model._
import   model.statement._
import   model.expression._

// Converts function call expressions to function call statements
// * Must be preceded by StatementBlockPass
// * Must be preceded by SideEffectPass
class FunctionCallStatementPass extends StatementVisitor {
  override def visit(stmt: ExpressionStatement): Int = {
    stmt.expr match {
      case e: FunctionCallExpr =>
        val blk = stmt.getParent.asInstanceOf[StatementBlock]
        blk.insertBefore(stmt, FunctionCallStatement(e))
        blk.removeStatement(stmt)
      case _ =>
    }
    
    return StatementVisitor.Continue
  }
}

object FunctionCallStatementPass {
  def doPass(tu: TranslationUnit): Unit = {
    StatementProcessor.process(tu, new FunctionCallStatementPass())
  }

  def doPass(function: FunctionDef): Unit = {
    StatementProcessor.process(function, new FunctionCallStatementPass())
  }
}
