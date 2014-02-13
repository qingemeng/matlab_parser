package   refactoring.matlab.processing

import   model._
import   model.expression._
import   model.statement._
import   refactoring.matlab.model._

object Preprocessor {
  class OpConverter extends ExpressionVisitor {
    override def visit(expr: NAryExpr): Int = {
//      if (expr.op == OpMatProd()) expr.update(op = OpTimes())
      if (expr.op == OpMatPow()) expr.update(op = OpPow())
      ExpressionVisitor.Continue
    }
  }
  
  def preprocess(ast: StatementBlock): StatementBlock = {
    // e.g. convert sin[] to functions sin()
    // convert all matmul ops ** to .*

	//val fn = ast.statements(0).asInstanceOf[FunctionDefStatement]
	//val blk = fn.funcDef.body.asInstanceOf[StatementBlock]
    val blk = ast
	
    StatementProcessor.process(blk, new StatementVisitor() {
      override def visit(stmt: AssignmentStatement): Int = {
        ExpressionProcessor.process(stmt.lhsExpr, new OpConverter)
        ExpressionProcessor.process(stmt.rhsExpr, new OpConverter)
        StatementVisitor.Continue
      }
    })
	blk
  }
}