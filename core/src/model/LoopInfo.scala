package   model

import   model.expression.Expr

object LoopInfo {
  def apply(
      inductionVar: IdName, 
      lowerBound: Expr, 
      upperBound: Expr, 
      stride: Expr) = new LoopInfo(inductionVar, lowerBound, upperBound, stride)
}

class LoopInfo(
    val inductionVar: IdName, 
    val lowerBound: Expr, 
    val upperBound: Expr, 
    val stride: Expr) {

  override def equals(that: Any): Boolean = that match {
    case o: LoopInfo => 
      this.inductionVar.equals(o.inductionVar) && 
      this.lowerBound.equals(o.lowerBound) &&
      this.upperBound.equals(o.upperBound) &&
      this.stride.equals(o.stride) 
    case _ => false
  }
  
  def pretty(hash: Boolean = false): String = {
    inductionVar.pretty(hash) + "(" + lowerBound.pretty(hash) + ":" + upperBound.pretty(hash) + ":" + stride.pretty(hash) + ")" 
  }
  
  def treePretty(level: Int = 0, hash: Boolean = false): String = {
    def indentStr(level: Int): String = "  " * level
    indentStr(level) + "LoopInfo: " + pretty(hash) + "\n" +
    indentStr(level) + "->InductionVar: " + inductionVar.treePretty(level+2, hash) +
    indentStr(level) + "->LowerBound: " + lowerBound.treePretty(level+2, hash) +
    indentStr(level) + "->UpperBound: " + upperBound.treePretty(level+2, hash) +
    indentStr(level) + "->Stride: " + stride.treePretty(level+2, hash) +
    "\n"
  }
}

/*
// indexVar - unique name that is a key in name map
case class LoopInfo(indexVar: String, lowerBound: Expr, upperBound: Expr, stride: Expr) {
  
//  val lbTerm = lowerBound.exprToTerm
//  val ubTerm = upperBound.exprToTerm
//  val strideTerm = stride.exprToTerm
//
//  val lbMap = Map(indexVar -> lbTerm)
//  val ubMap = Map(indexVar -> ubTerm)
//  val strideMap = Map(indexVar -> strideTerm)
  
  override def toString: String = {
    String.format("%s[%s, %s, %s]", indexVar, lowerBound.toString, upperBound.toString, stride.toString)
  }
}*/

// Assume that the loop is normalized to:
// for ([int] i = startexpr; i < endexpr; i++ or i+=n) OR
// for ([int] i = startexpr; i != endexpr; i++ or i+=n)?
// for ([int] i = startexpr; i > endexpr; i-- or i-=n)?
/*object LoopInfo {

  def extractFrom(fstmt: IASTForStatement, nameMap: NameMap): LoopInfo = {

    def convertToExpr(e: IASTExpression) = ExpressionTranslator.translate(e, nameMap)

    val lbExpr = convertToExpr(fstmt.getInitializerStatement match {
      case s: IASTDeclarationStatement =>
        s.getDeclaration().asInstanceOf[IASTSimpleDeclaration]
          .getDeclarators()(0).getInitializer().asInstanceOf[IASTEqualsInitializer]
          .getInitializerClause().asInstanceOf[IASTExpression]
      case s: IASTExpressionStatement => s.getExpression match {
        case e: IASTBinaryExpression => e.getOperand2()
      }
    })

    var indexVar: String = ""
    val ubExpr = fstmt.getConditionExpression match {
      case e: IASTBinaryExpression =>
        convertToExpr(e.getOperand1()) match {
          case IdExpr(n) => indexVar = n
          case _         => throw new UnsupportedOperationException
        }
        e.getOperator match {
          case IASTBinaryExpression.op_lessThan     => convertToExpr(e.getOperand2)
          case IASTBinaryExpression.op_lessEqual    => NAryExpr(OpPlus(), List(convertToExpr(e.getOperand2), ConstLiteralExpr(1)))
          case IASTBinaryExpression.op_greaterThan  => convertToExpr(e.getOperand2)
          case IASTBinaryExpression.op_greaterEqual => NAryExpr(OpPlus(), List(convertToExpr(e.getOperand2), UnaryExpr(OpMinus(), ConstLiteralExpr(1))))
        }
    }

    val stride = fstmt.getIterationExpression match {
      case e: IASTUnaryExpression => e.getOperator() match {
        case IASTUnaryExpression.op_postFixIncr => ConstLiteralExpr(1)
        case IASTUnaryExpression.op_prefixIncr  => ConstLiteralExpr(1)
        case IASTUnaryExpression.op_postFixDecr => UnaryExpr(OpMinus(), ConstLiteralExpr(1))
        case IASTUnaryExpression.op_prefixDecr  => UnaryExpr(OpMinus(), ConstLiteralExpr(1))
      }
      case e: IASTBinaryExpression => e.getOperator match {
        case IASTBinaryExpression.op_plusAssign =>
          ExpressionTranslator.translate(e.getOperand2(), nameMap)
        case IASTBinaryExpression.op_minusAssign =>
          val expr = ExpressionTranslator.translate(e.getOperand2(), nameMap)
          UnaryExpr(OpMinus(), expr)
      }
    }

    LoopInfo(indexVar, lbExpr, ubExpr, stride)
  }

}

*/