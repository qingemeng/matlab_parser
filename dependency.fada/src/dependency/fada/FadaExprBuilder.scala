package dependency.fada

import scala.collection.JavaConversions._
import com.hpctoday.fada._
import  model.expression._
import  model._

case class FadaExprBuilderFactory(generateArrays: Boolean) {
  def generateExpression(expr: Expr) = {
    val visitor = new FadaExprBuilder(generateArrays)
    ExpressionProcessor.process(expr, visitor)
    if (visitor.finalExpr == null) throw new UnsupportedExpressionException(expr.pretty())
    visitor.finalExpr
  }

  def generateCondition(expr: Expr) = {
    val visitor = new FadaExprBuilder(generateArrays)
    ExpressionProcessor.process(expr, visitor)
    if (visitor.finalCond == null) throw new UnsupportedConditionException(expr.pretty())
    visitor.finalCond
  }
}


class FadaExprBuilder(generateArrays: Boolean) extends ExpressionVisitor {
  var finalExpr: Expression = null
  var finalCond: Condition = null

  val factory = FadaExprBuilderFactory(generateArrays)
  def generateExpression(expr: Expr) = factory.generateExpression(expr)
  def generateCondition(expr: Expr) = factory.generateCondition(expr)

  override def visit(expr: NAryExpr): Int = {
    def generateBinaryExpressions(op: Expression.Operation) = {
      var rhs = generateExpression(expr.terms(0))
      for (term <- expr.terms.drop(1)) {
        rhs = new Expression(rhs, op, generateExpression(term))
      }
      rhs
    }

    def generateBinaryConditions(op: Condition.Logical_Operator) = {
      var rhs = generateCondition(expr.terms(0))
      for (term <- expr.terms.drop(1)) {
        rhs = new Condition(rhs, Condition.Logical_Operator.FADA_AND, generateCondition(term));
      }
      rhs
    }

    def generateInequation(op: Inequation.Predicate) = {
      new Condition(new Inequation(generateExpression(expr.terms(0)), op, generateExpression(expr.terms(1))))
    }

    expr.op match {
      case OpPlus() => finalExpr = generateBinaryExpressions(Expression.Operation.FADA_ADD)
      case OpTimes() => finalExpr = generateBinaryExpressions(Expression.Operation.FADA_MUL)
      case OpMinus() => finalExpr = generateBinaryExpressions(Expression.Operation.FADA_SUB)
      case OpDivide() => finalExpr = generateBinaryExpressions(Expression.Operation.FADA_DIV)
      case OpModulo() => finalExpr = generateBinaryExpressions(Expression.Operation.FADA_MOD)

      // Binary only
      //case OpShiftLeft() => 
      //case OpShiftRight() => 

      case OpLessThan() => finalCond = generateInequation(Inequation.Predicate.FADA_LESS)
      case OpLessEq() => finalCond = generateInequation(Inequation.Predicate.FADA_LESS_EQ)
      case OpGreaterThan() => finalCond = generateInequation(Inequation.Predicate.FADA_GREATER)
      case OpGreaterEq() => finalCond = generateInequation(Inequation.Predicate.FADA_GREATER_EQ)
      case OpEquals() => finalCond = generateInequation(Inequation.Predicate.FADA_EQ)
      case OpNotEquals() => finalCond = generateInequation(Inequation.Predicate.FADA_NEQ)

      // Binary and above
      //case OpBinaryAnd()	=> 
      //case OpBinaryXor()	=> 
      //case OpBinaryOr()		=> 

      case OpLogicalAnd() => finalCond = generateBinaryConditions(Condition.Logical_Operator.FADA_AND)
      case OpLogicalOr() => finalCond = generateBinaryConditions(Condition.Logical_Operator.FADA_OR)
      
      //Hack??
      case OpDotProd() => finalExpr = generateBinaryExpressions(Expression.Operation.FADA_MUL)
//      case OpOuterProd() => finalExpr = generateBinaryExpressions(Expression.Operation.FADA_MUL)
//      case OpMatProd()   => finalExpr = generateBinaryExpressions(Expression.Operation.FADA_MUL)

      case _ => throw new UnsupportedOperationException(expr.op.getClass() + " not supported")
    }

    ExpressionVisitor.Skip
  }

  override def visit(expr: UnaryExpr): Int = {
    val operand = generateExpression(expr.term)

    finalExpr = expr.op match {
      case OpNegate() => operand.mul(new Expression(-1));
      case OpInvert() => (new Expression(1)).div(operand)
      case _ => throw new UnsupportedOperationException(expr.op.getClass() + " not supported")
    }

    ExpressionVisitor.Skip
  }

  override def visit(expr: ArrayRefExpr): Int = {
    val arrayName = expr.owner.asInstanceOf[IdExpr].idName.name
    if(generateArrays){
      val indices = expr.indices.map(index => generateExpression(index))
      finalExpr = new Expression(Expression.Leaf.FADA_array, arrayName, indices)
    } else {
      finalExpr = new Expression(arrayName)
    }
    ExpressionVisitor.Skip
  }

  override def visit(expr: IdExpr): Int = {
    finalExpr = new Expression(expr.idName.name)
    ExpressionVisitor.Skip
  }

  override def visit(expr: ConstLiteralExpr): Int = {
    finalExpr = expr.kind match {
      case x: CharType => new Expression(expr.numeric.toChar())
      case x: ByteType => new Expression(expr.numeric.toByte())
      case x: ShortType => new Expression(expr.numeric.toShort())
      case x: IntType => new Expression(expr.numeric.toInt())
      case x: LongType => new Expression(expr.numeric.toInt())
      case x: FloatType => new Expression(expr.numeric.toInt())
      case x: DoubleType => new Expression(expr.numeric.toInt())
      case _ => throw new UnsupportedOperationException(expr.kind + " is not supported")
    }
    ExpressionVisitor.Skip
  }

  override def visit(expr: FunctionCallExpr): Int = {
    val arguments = expr.params.map(parm => generateExpression(parm))
    val funcName = expr.funcNameExpr.asInstanceOf[IdExpr].idName.name
    finalExpr = new Expression(Expression.Leaf.FADA_function, funcName, arguments)

    ExpressionVisitor.Skip
  }

  override def visit(expr: FieldRefExpr): Int = { ExpressionVisitor.Skip }
  override def visit(expr: ExpressionListExpr): Int = { ExpressionVisitor.Skip }
  override def visit(expr: ConditionalExpr): Int = { ExpressionVisitor.Skip }
  override def visit(expr: TypeIdExpr): Int = { ExpressionVisitor.Skip }
  override def visit(expr: AllocateArrayExpr): Int = { ExpressionVisitor.Skip }
  override def visit(expr: DeallocateArrayExpr): Int = { ExpressionVisitor.Skip }
}