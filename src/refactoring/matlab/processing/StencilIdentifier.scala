package   refactoring.matlab.processing

import   core.Algebra
import   model._
import   model.expression._
import   model.statement._


protected class StencilSpec {
  var isStencil: Boolean = false
  var statement: AssignmentStatement = null
  var index: Int = 0
  var name: String = null
  //var tempOutName: String = null
  var radius: Int = 0
  // list of coefficient arrays
  var arraysCoeff: Map[String, ArrayRefExpr] = Map.empty
  // list of stencil arrays
  var arraysIn: Map[String, ArrayRefExpr] = Map.empty
  var arrayOut: ArrayRefExpr = null
  var tileX: Int = 128
  var tileY: Int = 4
  var regX: Int = 1
  var regY: Int = 1
  
  def requiresTemp: Boolean = {
    val outId = arrayOut.owner.asInstanceOf[IdExpr].idName.id
    arraysIn contains outId
  }
  def outName: String = {
    if (requiresTemp) {
      val outId = arrayOut.owner.asInstanceOf[IdExpr].idName.id
      s"${outId}_t"
    } else {
      arrayOut.owner.asInstanceOf[IdExpr].idName.id
    }
  }
}

object StencilIdentifier {
  def identify(stmt: Statement) = {
    val visitor = new StencilIdentifierVisitor()
    
    StatementProcessor.process(stmt, visitor)
    
    visitor.stencils
  }
}

class StencilIdentifierVisitor extends StatementVisitor {
  var stencils: Map[AssignmentStatement, StencilSpec] = Map.empty
  
  override def visit(stmt: AssignmentStatement): Int = {
    val spec = checkStencilStatement(stmt)
    
    if (spec.isStencil)
      stencils += stmt -> spec
    
    StatementVisitor.Continue
  }
  
  protected def checkValidArraySlices(array: ArrayRefExpr): Boolean =  {
    array.indices.forall(i => i.isInstanceOf[SliceExpr])
  }
  
  // extract domain's lower bound from slice expression
  protected def extractLB(expr: Expr): List[Expr] = expr match {
	case e: ArrayRefExpr =>
	  e.indices.reverse.foldLeft(List.empty[Expr]) {
	    case (res, i: SliceExpr) => i.lowerBound :: res
	  }
	case _ => List.empty[Expr]
  }
  protected def subtractLB(dom1: List[Expr], dom2: List[Expr]): List[Expr] = {
    (dom1 zip dom2).map {
      case (d1, d2) => Algebra.simplify(d1 - d2)
    }    
  }
  
  protected def checkStencilStatement(stmt: AssignmentStatement): StencilSpec = {
    // Compare domains of RHS with LHS to determine if statement is stencil
    // Limitations:
    // - assume LHS and RHS arrays have same sizes
    // - array index must be all SliceExpr
    // - isStencil is true as long as there is integer difference between 2 SliceExpr of same array
    // - Only uses lower bound to check
    
    val spec = new StencilSpec
    if (!stmt.lhsExpr.isInstanceOf[ArrayRefExpr]) {
      return spec
    }
    val array = stmt.lhsExpr.asInstanceOf[ArrayRefExpr]
    if (!checkValidArraySlices(array)) {
      return spec      
    }
    
    spec.statement = stmt
    spec.arrayOut = stmt.lhsExpr.asInstanceOf[ArrayRefExpr]
    val outId = spec.arrayOut.owner.asInstanceOf[IdExpr].idName.id
    val lhsDom = extractLB(stmt.lhsExpr)
    
    ExpressionProcessor.process(stmt.rhsExpr, new ExpressionVisitor() {
      override def visit(expr: ArrayRefExpr): Int = {
        // checks validity, all index must be SliceExpr
        if (checkValidArraySlices(expr)) {
          val rhsDom = extractLB(expr)
          val diffDom = subtractLB(rhsDom, lhsDom)
          // obtain maximum distance
          val maxAbs = diffDom.map (diff => 
            diff match {
              case e: ConstLiteralExpr if e.isNumeric => math.abs(e.numeric.toInt)
              case _ => 0
            } 
          ).max
          val id = expr.owner.asInstanceOf[IdExpr].idName.id
          if (maxAbs > spec.radius) {
            spec.radius = maxAbs
            spec.isStencil = true
            spec.arraysIn += (id -> expr)  
          } 
          if (maxAbs == 0 && id != outId) {
        	spec.arraysCoeff += (id -> expr)
          }
        }
        
        ExpressionVisitor.Continue 
      }
    })
    
    spec.index = stencils.size	// start from 0
    spec.name = s"stencil${stencils.size}"
    spec
  }
  
  
}