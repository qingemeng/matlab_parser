package   core.processing

import   core._
import   model._
import   model.statement._
import   model.expression._

// Modifies the expression so that
//  - No OpMinus or OpDivide operators
//  - Legal NAry operators are merged
//
// * Only handles AssignmentStatements
// * Must be preceded by StatementBlockPass
// * Should be preceded by SideEffectPass
// * Must be preceded by AssignmentStatementPass
class ExpressionNormalizationPass extends StatementVisitor {
  /* Moved to LoopSplittingPass
  override def leave(stmt: ForStatement): Unit = {
    val visitor = new NormalizeExprVisitor()
    // XXX: should have a pass to normalize init stmt?
    // XXX: HACK only certain fors are handled
    if (!stmt.initStmt.isEmpty) {
      stmt.initStmt.get match {
        case DeclarationStatement(List(decl @ Declarator(idName, Some(xpr)))) => 
          var e = ExpressionProcessor.process(xpr, visitor)
          e = Algebra.mergeOperators(e)
          decl.update(initializer = Some(e))
        case ExpressionStatement(s @ NAryExpr(OpTempAssign(), List(IdExpr(idName), xpr))) => 
          var e = ExpressionProcessor.process(xpr, visitor)
          e = Algebra.mergeOperators(e)
          s.update(terms = List(IdExpr(idName), e))
        case _ => throw new UnsupportedOperationException
      }
    }
    if (!stmt.condExpr.isEmpty) {
      var condExpr = ExpressionProcessor.process(stmt.condExpr.get, visitor)
      condExpr = Algebra.mergeOperators(condExpr)
      stmt.update(condExpr = Some(condExpr))
    }
    if (!stmt.iterExpr.isEmpty) {
      var iterExpr = ExpressionProcessor.process(stmt.iterExpr.get, visitor)
      iterExpr = Algebra.mergeOperators(iterExpr)
      stmt.update(iterExpr = Some(iterExpr))
    }
  }*/
  
  override def leave(stmt: AssignmentStatement): Unit = {
    val visitor = new NormalizeExprVisitor()
    // normalize lhs and rhs, including indices inside ArrayRefExpr
    var lhs = stmt.lhsExpr
    var rhs = stmt.rhsExpr
    lhs = ExpressionProcessor.process(lhs, visitor)
    lhs = Algebra.mergeOperators(lhs)
    stmt.update(lhsExpr = lhs)
    rhs = ExpressionProcessor.process(rhs, visitor)
    rhs = Algebra.mergeOperators(rhs)
    stmt.update(rhsExpr = rhs)
  }
}

object ExpressionNormalizationPass {
  def doPass(tu: TranslationUnit): Unit = {
    StatementProcessor.process(tu, new ExpressionNormalizationPass())
  }
  def doPass(function: FunctionDef): Unit = {
    StatementProcessor.process(function, new ExpressionNormalizationPass())
  }
  def doPass(statement: Statement): Unit = {
    StatementProcessor.process(statement, new ExpressionNormalizationPass())
  }
}

// Convert OpMinus and OpDivide binary operators to OpNegate and OpInvert
class NormalizeExprVisitor extends ExpressionVisitor {
  override def leave(expr: NAryExpr): Expr = {
    if(expr.terms.size == 1){
      expr.terms(0)
    } else {
      val lhs = expr.terms(0)
      val rhs = expr.terms(1)
      expr.op match {
        //XXX:case OpMinus()  => NAryExpr(OpPlus(), List(lhs, UnaryExpr(OpNegate(), rhs)))
        //XXX: try -1*x
        case OpMinus()  => {
          val newRhs = rhs match {
            case e: ConstLiteralExpr if e.isNumeric => ConstLiteralExpr(-e.numeric) 
            case _ => NAryExpr(OpTimes(), List(ConstLiteralExpr(-1), rhs))
          }
          NAryExpr(OpPlus(), List(lhs, newRhs))
        }
        case OpDivide() => lhs match {
          case e: ConstLiteralExpr => 
            if (e.isNumeric && e.numeric == 1) UnaryExpr(OpInvert(), rhs)
            else NAryExpr(OpTimes(), List(lhs, UnaryExpr(OpInvert(), rhs)))
          case _ => NAryExpr(OpTimes(), List(lhs, UnaryExpr(OpInvert(), rhs)))
        }
        case _ => expr
      }
    }
  }
  
  override def leave(expr: UnaryExpr): Expr = expr.op match {
    case OpNegate() => expr.term match {
      case e: ConstLiteralExpr if e.isNumeric => ConstLiteralExpr(-e.numeric) 
      case _ => expr
    }
    case _ => expr
  }
}