package   core.processing

import   core._
import   model._
import   model.statement._
import   model.expression._

//Convert Miscellaneous Function Call Expressions to Operators 
class MiscFunctionToOpPass extends ExpressionVisitor {
  override def leave(expr: FunctionCallExpr): Expr = {
    if(expr.funcNameExpr.isInstanceOf[IdExpr] && expr.params.length > 0){
      def checkNaryParams(op: NAryOp) = {
        if(expr.params.length > 1){
          NAryExpr(op, expr.params)
        } else {
          expr
        }
      }
      
      expr.funcNameExpr.asInstanceOf[IdExpr].idName.name match {
        case "Transpose"  => UnaryExpr(OpTranspose(), expr.params(0))
        
        case "abs"  => UnaryExpr(OpAbs(), expr.params(0))
        case "sqrt" => UnaryExpr(OpSqrt(), expr.params(0))
        case "log"  => UnaryExpr(OpLog(), expr.params(0))
        case "expr" => UnaryExpr(OpExp(), expr.params(0))
        case "sin"  => UnaryExpr(OpSin(), expr.params(0))
        case "cos"  => UnaryExpr(OpCos(), expr.params(0))
        case "tan"  => UnaryExpr(OpTan(), expr.params(0))
        case "asin" => UnaryExpr(OpASin(), expr.params(0))
        case "acos" => UnaryExpr(OpACos(), expr.params(0))
        
        case "ceil" => UnaryExpr(OpCeil(), expr.params(0))
        case "floor" => UnaryExpr(OpFloor(), expr.params(0))
        
        case "pow" => checkNaryParams(OpPow())
        case "min" => checkNaryParams(OpMin())
        case "max" => checkNaryParams(OpMax())
        
        case _ => expr
      }
    } else {
      expr
    }
  }
}

object MiscFunctionToOpPass {
  def doPass(tu: TranslationUnit): Unit = {
    ExpressionProcessor.process(tu, new MiscFunctionToOpPass())
  }

  def doPass(function: FunctionDef): Unit = {
    ExpressionProcessor.process(function, new MiscFunctionToOpPass())
  }
  
  def doPass(stmt: Statement): Unit = {
    ExpressionProcessor.process(stmt, new MiscFunctionToOpPass())
  }
}