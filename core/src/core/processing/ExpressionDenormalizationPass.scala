package   core.processing

import   core._
import   model._
import   model.statement._
import   model.expression._

// Modifies the expression so that
//  - x + (-1 * y) becomes x - y
//  - -1 * x becomes unary -x
//
// * Only handles AssignmentStatements
// * Must be preceded by StatementBlockPass
// * Should follow LiftingPass
class ExpressionDenormalizationPass extends StatementVisitor {
  override def leave(stmt: AssignmentStatement): Unit = {
    val visitor = new DenormalizeExprVisitor()
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

object ExpressionDenormalizationPass {
  def doPass(tu: TranslationUnit): Unit = {
    StatementProcessor.process(tu, new ExpressionDenormalizationPass())
  }
  def doPass(function: FunctionDef): Unit = {
    StatementProcessor.process(function, new ExpressionDenormalizationPass())
  }
  def doPass(statement: Statement): Unit = {
    StatementProcessor.process(statement, new ExpressionDenormalizationPass())
  }
}

// Convert back to using OpMinus binary operator
// TODO: OpInvert to OpDivide not yet implemented
class DenormalizeExprVisitor extends ExpressionVisitor {
  override def leave(expr: NAryExpr): Expr = {
    expr.op match {
      case OpPlus() =>
        // check if child has -c * x terms
        // partition plus and minus terms
        // keep on making OpMinus tree
//        var curTerm: Expr = null
        var plusTerms = List.empty[Expr]
        var minusTerms = List.empty[Expr]
        for (e <- expr.terms) {
          e match {
            case child: NAryExpr if child.op == OpTimes() =>
              var (constPart, nonConstPart) = child.terms.partition(Algebra.isNumericConst)
              nonConstPart = nonConstPart.sortBy(Algebra.lexOrder)
              val constExpr = Algebra.multiplyConsts(constPart)
              val constValue = constExpr.numeric
              val absExpr = if (constValue == -1) {
            	  			  if (nonConstPart.size == 0) ConstLiteralExpr(1)
            	  			  else if (nonConstPart.size == 1) nonConstPart(0)
            	  			  else NAryExpr(OpTimes(), nonConstPart)
              				} else NAryExpr(OpTimes(), ConstLiteralExpr(-constValue)::nonConstPart)
              if (constValue < 0) {
                minusTerms = absExpr::minusTerms
//                if (curTerm == null) {
//                  curTerm = absExpr
//                } else {
//                  curTerm = NAryExpr(OpMinus(), List(curTerm, absExpr))
//                }
              } else {
                plusTerms = e::plusTerms
              }
            case _ => plusTerms = e::plusTerms
          }
        }
        // build tree
        //println(PrettyPrinter.pretty(expr))
        //for (e <- plusTerms) println(PrettyPrinter.pretty(e))
        //for (e <- minusTerms) println(PrettyPrinter.pretty(e))
        plusTerms = plusTerms.reverse
        minusTerms = minusTerms.reverse
        var finalTree: Expr = null
        var firstTerm = plusTerms.size match {
          case 0 => UnaryExpr(OpNegate(), minusTerms(0))
          case 1 => plusTerms(0)
          case _ => NAryExpr(OpPlus(), plusTerms)
        }
        finalTree = firstTerm
        for (minusTerm <- minusTerms) {
          finalTree = NAryExpr(OpMinus(), List(finalTree, minusTerm))
        }
        //println(PrettyPrinter.pretty(finalTree))
        return finalTree
//        if (curTerm == null) expr 
//        else {
//          // add the plus terms
//          //println(PrettyPrinter.pretty(NAryExpr(OpPlus(), (curTerm::plusTerms).reverse)))
//          if (plusTerms.size != 0) NAryExpr(OpPlus(), (curTerm::plusTerms).reverse.sortBy(Algebra.lexOrder))
//          else curTerm
//        }
      case _ => expr
    }
  }
}