package dependency.fada

import scala.collection.JavaConversions._
import com.hpctoday.fada._
import  model._
import  model.expression._

//Convert Fada Expression to DSL Expression
object DslExprBuilder {
  def generateExpression(expr: Expression):Expr = {
    if(expr.IsLeaf()){
	  if(expr.IsValue()){
	    ConstLiteralExpr(expr.GetValue())
	  } else if(expr.IsArray()){
	    ArrayRefExpr(IdExpr(IdName(expr.GetArrayName())), expr.GetIndex().map(param => generateExpression(param)).toList)
	  } else if(expr.IsFunction()){
	    FunctionCallExpr(IdExpr(IdName(expr.GetFunctionName())), expr.GetArguments().map(param => generateExpression(param)).toList)
	  } else if(expr.IsVariable()){
	    IdExpr(IdName(expr.GetVariableName()))
	  } else {
	    throw new UnsupportedOperationException("Cannot convert Fada Expression: " + expr.Generate_C_Code())
	  }
    } else {
      val op = expr.GetOperation() match {
        case Expression.Operation.FADA_ADD => OpPlus()
    		case Expression.Operation.FADA_SUB => OpMinus()
    		case Expression.Operation.FADA_MUL => OpTimes()
    		case Expression.Operation.FADA_DIV => OpDivide()
    		case Expression.Operation.FADA_MOD => OpModulo()
      }
      
      val terms = List[Expr](generateExpression(expr.GetLeftChild()), generateExpression(expr.GetRightChild()))
      NAryExpr(op, terms)
    }
  }
}