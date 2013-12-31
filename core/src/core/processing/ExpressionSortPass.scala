package   core.processing

import scala.collection.mutable.ListBuffer

import   model._
import   model.expression._
import   model.statement._

object ExpressionSortPass {
  object Sorter extends ExpressionVisitor(){
    override def leave(expr: NAryExpr): Expr = expr.op match {
      case OpPlus() => sort(expr)
      case OpTimes() => sort(expr)
      case _ => expr
    }
  }  
  
  def toPass(func: FunctionDef) = ExpressionProcessor.process(func, Sorter)
  def toPass(stmt: Statement) = ExpressionProcessor.process(stmt, Sorter)
  def toPass(expr: Expr): Expr = ExpressionProcessor.process(expr, Sorter)
  
  def sort(expr: NAryExpr): Expr = {
    val const = ListBuffer.empty[Expr]
    val nonConst = ListBuffer.empty[Expr]
    val others = ListBuffer.empty[Expr]
    
    expr.terms.foreach(_ match {
      case c: ConstLiteralExpr => const += c
      case e: IdExpr => nonConst += e
      case e: ArrayRefExpr => nonConst += e
      case e @ UnaryExpr(OpNegate(), IdExpr(_)) => nonConst += e
      case e @ UnaryExpr(OpNegate(), ArrayRefExpr(_)) => nonConst += e
      case o => others += o
    })
    
    val xprs = (nonConst.sortBy(lexOrder).reverse ++ const.sortBy(lexOrder).reverse ++ others.sortBy(lexOrder).reverse).toList
    
    //val xprs = expr.terms.sortBy(lexOrder).reverse
    //println("SORTED: " + xprs.map(_.pretty()).mkString(", "))
    expr.update(terms = xprs)
  }
  
  // To be used for leaf nodes only
  def lexOrder(expr: Expr): String = {
//    expr match {
//      case e: ConstLiteralExpr => "zz~1"
//      case e: NAryExpr => {
//        val xprs = e.terms.sortBy(lexOrder)
//        NAryExpr(OpMinus(), xprs).pretty()
//      }
//      case e => e.pretty()
//    }
    expr.pretty()
  }

//  def isNumericConst(expr: Expr): Boolean = {
//    expr match {
//      case e: ConstLiteralExpr => e.isNumeric 
//      case _ => false
//    }
//  }
  
}

