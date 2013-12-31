package model.expression


import   core._
import   model._
import   model.expression._

object ExprHelper {
  // Normalize, simplify, denormalize
//  def simplify(expr: Expr) = {
//    //println(PrettyPrinter.pretty(expr))
//    val normVisitor = new NormalizeExprVisitor()
//    val denormVisitor = new DenormalizeExprVisitor()
//    var xpr = ExpressionProcessor.process(expr, normVisitor)
//    xpr = Algebra.simplify(xpr)
//    xpr = ExpressionProcessor.process(xpr, denormVisitor)
//    xpr
//  }
  
  def isLess(expr1: Expr, expr2: Expr): Boolean = {
    Algebra.simplify(expr1 - expr2) match {
      case e: ConstLiteralExpr => e.numeric < 0
      case _ => false
    }  
  }
  def isLessEqual(expr1: Expr, expr2: Expr): Boolean = {
    Algebra.simplify(expr1 - expr2) match {
      case e: ConstLiteralExpr => e.numeric <= 0
      case _ => false
    }  
  }
  def isGreater(expr1: Expr, expr2: Expr): Boolean = {
    Algebra.simplify(expr1 - expr2) match {
      case e: ConstLiteralExpr => e.numeric > 0
      case _ => false
    }  
  }
  def isGreaterEqual(expr1: Expr, expr2: Expr): Boolean = {
    Algebra.simplify(expr1 - expr2) match {
      case e: ConstLiteralExpr => e.numeric >= 0
      case _ => false
    }  
  }  
}