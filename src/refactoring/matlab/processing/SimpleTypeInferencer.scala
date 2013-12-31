package   refactoring.matlab.processing

import   model._
import   model.expression._
import   model.statement._
import scala.collection.mutable

// Given a block of code, infers the type from first set ref
// Limitations:
// - default to float
// - flat scope (vars are unique)
// - zeros(1, N) is treated as maxtrix
// - ArrayCompositionExpr not handled
object SimpleTypeInferencer {
  
  def infer(ast: Statement) = {
    val typeInfo = new SimpleTypeInferencer().infer(ast)
    
    //typeInfo.foreach(t => println(s"${t._1} ~ ${t._2}"));println
    
    typeInfo
  }
  
}

class SimpleTypeInferencer {
  
  def infer(ast: Statement) = {
    val typer = new Typer
    
    val visitor = new TypeStatementVisitor(typer)
    
    StatementProcessor.process(ast, visitor)
    
    typer.vars.toMap
  }
}

// Utility to merge types
private class Typer {
  val vars: mutable.Map[String, BasicType] = mutable.Map.empty
  
  def updateType(id: String, idType: BasicType): BasicType = {
    if (vars contains id) {
      val oldType = vars(id)
      val newType = mergeType(oldType, idType)
      vars(id) = newType
    } else {
      vars += (id -> idType)
    }
    
    vars(id)
  }
  
  // merge types
  // null represent unknown type
  def mergeType(type1: BasicType, type2: BasicType): BasicType = {
    if (type1 == null && type2 == null) 
      return null
    
    if (type1 == null)
      return type2
      
    if (type2 == null)
      return type1
    
    type1 match {
      case t1: IntType => type2 match {
        case t2: IntType   => return t1
        case t2: FloatType => ???
        case t2: ArrayType => ???
        case _ => ???
      }
      case t1: FloatType => type2 match {
        case t2: IntType   => return t1
        case t2: FloatType => return t1
        case t2: ArrayType => return t2
        case _ => ???
      }
      case t1: ArrayType => type2 match {
        case t2: IntType => return t1
        case t2: FloatType   => return t1
        case t2: ArrayType =>
          // currently does not test for sizes equality
          // what happens when t1=Array[X][Y] and t2=Array[A][B]? 
          // take t1 since we assume array is defined before use, i.e. t1 comes first before t2
          //if (t1.rank != t2.rank) throw new UnsupportedOperationException
          if (t1.sizes.isEmpty) return t2
          if (t2.sizes.isEmpty) return t1
          return t1
      }
      case _ => ???
    }
  }
  
}

private class TypeStatementVisitor(typer: Typer) extends StatementVisitor {
  
  override def visit(stmt: AssignmentStatement): Int = {
	// Assumption: rhs vars are defined before use
    
    val lhsVisitor = new TypeExprVisitor(typer)
    val rhsVisitor = new TypeExprVisitor(typer)
    
    ExpressionProcessor.process(stmt.lhsExpr, lhsVisitor)
    ExpressionProcessor.process(stmt.rhsExpr, rhsVisitor)

    // obtain id
    val id = stmt.lhsExpr match {
      case e: IdExpr       => e.idName.id
      case e: ArrayRefExpr => e.owner.asInstanceOf[IdExpr].idName.id
    }
    typer.updateType(id, rhsVisitor.exprType)
    //val exprType = typer.mergeType(lhsVisitor.exprType, rhsVisitor.exprType)
    
    //vars += (lhs -> exprType)
    
    return StatementVisitor.Continue
  }
} 

private class TypeExprVisitor(typer: Typer) extends ExpressionVisitor {
  
  val defaultType = FloatType()
  var exprType: BasicType = null

  override def visit(expr: IdExpr): Int = {
//    if (vars contains expr.idName.id) {
//      val idType = vars(expr.idName.id)
//      exprType = SimpleTypeInferencer.mergeType(exprType, idType)
//    } else {
//      vars += (expr.idName.id -> null)
//    } 
    // no type information
	//exprType = typer.updateType(expr.idName.id, defaultType)
    return ExpressionVisitor.Continue
  }
  
  override def visit(expr: FunctionCallExpr): Int = {
    val fn = expr.funcNameExpr.asInstanceOf[IdExpr].idName.id
    
    if (fn == "zeros" || fn == "ones") {
      val rank = expr.params.size
      val sizes = expr.params
      exprType = typer.mergeType(exprType, ArrayType(defaultType, rank, sizes))
    } else if (fn == "floor" || fn == "ceil" || fn == "exp") {
      exprType = typer.mergeType(exprType, defaultType)
    }
    
    return ExpressionVisitor.Skip  
  }
  
  override def visit(expr: ConstLiteralExpr): Int = {
    expr.kind match {
      case t: IntType     => exprType = typer.mergeType(exprType, t)
      case t: FloatType   => exprType = typer.mergeType(exprType, t)
      case t: DoubleType  => exprType = typer.mergeType(exprType, t)
      case t: StringType  => throw new UnsupportedOperationException
      case _ => println(expr.kind); throw new UnsupportedOperationException
    }
    
    return ExpressionVisitor.Continue
  }
  
  override def visit(expr: ArrayRefExpr): Int = {
    // we cannot determine the sizes from accessing a sub-array
    // array type is default to float
    exprType = typer.mergeType(exprType, ArrayType(defaultType, expr.rank, List.empty))
    
    return ExpressionVisitor.Continue
  }
  
}