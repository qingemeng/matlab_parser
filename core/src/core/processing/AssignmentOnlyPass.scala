package   core.processing

import   core._
import   model._
import   model.statement._
import   model.expression._

//Convert OpAssign to Assignment only
class AssignmentOnlyPass extends StatementVisitor {
  override def leave(stmt: AssignmentStatement): Unit = {
    def wrapNewExpr(op: NAryOp){
      val rhsExpr = NAryExpr(op, List(stmt.lhsExpr.cloneExpr(), stmt.rhsExpr))
	  stmt.update(stmt.lhsExpr, rhsExpr, OpAssign())
    }
    
    def transform(op: NAryOp){
      if(stmt.rhsExpr.isInstanceOf[NAryExpr]){
        val rhsExpr = stmt.rhsExpr.asInstanceOf[NAryExpr]

        //Same operator, no need to create new expression
        if(rhsExpr.op == op){
          rhsExpr.update(op, List(stmt.lhsExpr.cloneExpr()) ++ rhsExpr.terms)
          stmt.update(stmt.lhsExpr, rhsExpr, OpAssign())
        } else {
          wrapNewExpr(op)
        }
      } else {
        wrapNewExpr(op)
      }
    }
    
    stmt.assignOp match {
      case OpAssign() =>
  	  case OpPlusAssign() => transform(OpPlus())
  	  case OpMinusAssign() => transform(OpMinus())
  	  case OpTimesAssign() => transform(OpTimes())
  	  case OpDivideAssign() => transform(OpDivide())
  	  
  	  case _ =>
    }
  }
}

object AssignmentOnlyPass {
  def doPass(stmt: Statement): Unit = {
    StatementProcessor.process(stmt, new AssignmentOnlyPass())
  }
  def doPass(tu: TranslationUnit): Unit = {
    StatementProcessor.process(tu, new AssignmentOnlyPass())
  }
  def doPass(function: FunctionDef): Unit = {
    StatementProcessor.process(function, new AssignmentOnlyPass())
  }
}