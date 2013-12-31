package   core.processing

import   core._
import   model._
import   model.expression._
import   model.statement._

object LoopHelper {
  def hasValidForHeader(stmt: ForStatement): Option[LoopInfo] = {
    // Currently, we handle loops of the form
    // For (i=s; i<N; i++ or i+=k)
    // Extract index variable, start and end values, stride

    if (stmt.initStmt.isEmpty || stmt.condExpr.isEmpty || stmt.iterExpr.isEmpty) return None
    
    var inductionVar: IdName = null
    var lbExpr: Expr = null
    var ubExpr: Expr = null
    var stride: Expr = null
    
    stmt.condExpr.get match {
      case NAryExpr(OpLessThan(), List(IdExpr(idName), xpr)) => inductionVar = idName; ubExpr = xpr
      case NAryExpr(OpLessEq(), List(IdExpr(idName), xpr))   => inductionVar = idName; ubExpr = xpr + 1
      case _ => return None
    }
    
    stmt.initStmt.get match {
      case DeclarationStatement(List(Declarator(idName, Some(xpr))))                => if (idName != inductionVar) return None else lbExpr = xpr
      case ExpressionStatement(NAryExpr(OpTempAssign(), List(IdExpr(idName), xpr))) => if (idName != inductionVar) return None else lbExpr = xpr
      case AssignmentStatement(IdExpr(idName), xpr, OpAssign()) => if (idName != inductionVar) return None else lbExpr = xpr
      case _ => return None
    }

    stmt.iterExpr.get match {
      case UnaryExpr(OpPrefixInc(), IdExpr(idName))                => if (idName != inductionVar) return None else stride = ConstLiteralExpr(1)
      case UnaryExpr(OpPrefixDec(), IdExpr(idName))                => if (idName != inductionVar) return None else stride = ConstLiteralExpr(-1)
      case UnaryExpr(OpPostfixInc(), IdExpr(idName))               => if (idName != inductionVar) return None else stride = ConstLiteralExpr(1)
      case UnaryExpr(OpPostfixDec(), IdExpr(idName))               => if (idName != inductionVar) return None else stride = ConstLiteralExpr(-1)
      case NAryExpr(OpTempPlusAssign(), List(IdExpr(idName), xpr)) => if (idName != inductionVar) return None else stride = xpr
      case _ => return None
    }
    
    // Normalize and simplify the expressions
    lbExpr = Algebra.simplify(lbExpr)
    ubExpr = Algebra.simplify(ubExpr)
    stride = Algebra.simplify(stride)
    Some(LoopInfo(inductionVar, lbExpr, ubExpr, stride))  
  }
}