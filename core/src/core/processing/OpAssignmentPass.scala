package   core.processing

import   core._
import   model._
import   model.statement._
import   model.expression._

//Reverse the effects of AssignmentOnlyPass
class OpAssignmentPass extends StatementVisitor {
  override def leave(stmt: AssignmentStatement): Unit = {
    if(stmt.assignOp == OpAssign() && stmt.rhsExpr.isInstanceOf[NAryExpr]){
      val lhsExpr = stmt.lhsExpr
      val rhsExpr = stmt.rhsExpr.asInstanceOf[NAryExpr]
      
      val newAssignOp = convertOp(rhsExpr.op)
      if(rhsExpr.terms.contains(lhsExpr) && (newAssignOp ne null)){
        val newTerms = rhsExpr.terms diff List(lhsExpr)
        
        val newRhs: Expr = 
          newTerms.size match {
            case 0 => ???
            case 1 => newTerms(0)
            case _ => NAryExpr(rhsExpr.op, newTerms)
          }
        
        stmt.update(rhsExpr = newRhs, op = newAssignOp)
      }
    }
  }
  
  private def convertOp(op: NAryOp): AssignOp = op match {
    case OpPlus() => OpPlusAssign()
    case OpMinus() => OpMinusAssign()
    case OpTimes() => OpTimesAssign()
    case OpDivide() => OpDivideAssign()
    case _ => null
  }
}

object OpAssignmentPass {
  def doPass(tu: TranslationUnit): Unit = {
    StatementProcessor.process(tu, new OpAssignmentPass())
  }
  def doPass(function: FunctionDef): Unit = {
    StatementProcessor.process(function, new OpAssignmentPass())
  }
  def doPass(stmt: Statement): Unit = {
    StatementProcessor.process(stmt, new OpAssignmentPass())
  }  
}