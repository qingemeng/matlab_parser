package   refactoring.matlab.processing

import   model._
import   model.expression._
import   model.statement._
import scala.collection.mutable
import scala.util.control.Breaks
import core.util._

// Given a block of code, infers the type from first set ref
// Limitations:
// - default to float
// - flat scope (vars are unique)
// - zeros(1, N) is treated as maxtrix
// - ArrayCompositionExpr not handled
object SimpleTypeInferencer {

  def infer(ast: Statement) = {
    val typeInfo = new SimpleTypeInferencer().infer(ast)//,level)

    //typeInfo.foreach(t => println(s"${t._1} ~ ${t._2}"));println

    typeInfo
  }

}

class SimpleTypeInferencer {

  def infer(ast: Statement) = {//,level:Int) = {
  val typer = new Typer

    val visitor = new TypeStatementVisitor(typer)

    StatementProcessor.process(ast, visitor)

    typer.vars.toMap
  }
}

// Utility to merge types
 class Typer {
  val vars: mutable.Map[String, BasicType] = mutable.Map.empty
  //  val rhs_vars:mutable.Map[String, BasicType] = mutable.Map.empty

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
  // check all id at rhs is declared

  //if each ele of an array has the same data type
  // array eles are array type recursive check

  def getRank(exprs:List[Expr]):Int={
    val ranks = exprs.map {
      case e: ArrayCompositionExpr => getRank(e.exprs) + 1
      case e: ArrayRefExpr => e.rank + 1
      case _ => 1
    }
    ranks.max
  }

  def getSize(exprs:List[Expr]):List[Expr]={
    val sizes = exprs match {
      case e: List[IdExpr] =>e
      case e: List[ConstLiteralExpr]=>e
      case e: List[ArrayCompositionExpr]=> getSize(e.head.exprs)
      case e: List[ArrayRefExpr] => e.head.indices
      case _ => List.empty
    }
    //    val sizes = exprs.map  {
    //      case e: ArrayCompositionExpr=> getSize(e.exprs).head
    //      case e: ArrayRefExpr => e.indices.head
    //      case e: IdExpr =>e
    //      case e: ConstLiteralExpr=>e
    //      case _ => List.empty.head
    //    }
    println("sizes = " + sizes.size)

    sizes
  }


  def getArrSubType(expr:ArrayCompositionExpr): BasicType = {
    getType(getIds( expr.exprs).head)
  }



  // merge types

  def arrIsValid(arrayType: ArrayType) :Boolean = {
      ErrHelper(this).arrIsValid(arrayType)
  }


  def canBeMerged(t1: ArrayType, t2: ArrayType): Boolean= {
    ErrHelper(this).canBeMerged(t1,t2)
  }

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
        case t2: FloatType => return t2
        case t2: ArrayType => {

          if(t2!= null && t2.rank==1 && t2.sizes.length==1){
            return mergeType(t1, getType(getIds( t2.sizes).head) )
          }
          else
            return NullType()

        }
        case t2: BooleanType => t1
        case _ => NullType()
      }
      case t1: FloatType => type2 match {
        case t2: IntType   => return t1
        case t2: FloatType => return t1
        case t2: ArrayType => {

          if(t2!= null && t2.rank==1 && t2.sizes.length==1){
            return mergeType(t1, getType(getIds( t2.sizes).head) )
          }
          else
            return NullType()

        }
        case t2: BooleanType => return IntType()
        case _ => NullType()
      }
      case t1: ArrayType => type2 match {
        case t2: IntType => {

          if(t1!= null && t1.rank==1 && t1.sizes.length==1){
            return mergeType(t2,t1.subType)
          }
          else
            return NullType()

        }
        case t2: FloatType    => {

          if(t1!= null && t1.rank==1 && t1.sizes.length==1){
            return mergeType(t2, t1.subType )
          }
          else
            return NullType()

        }
        case t2: ArrayType =>  {
          if (t1.sizes.isEmpty) return t2
          if (t2.sizes.isEmpty) return t2
          if(canBeMerged(t1,t2) )
            return t1

          return NullType()
        }
        // currently does not test for sizes equality
        // what happens when t1=Array[X][Y] and t2=Array[A][B]? 
        // take t1 since we assume array is defined before use, i.e. t1 comes first before t2
        //if (t1.rank != t2.rank) throw new UnsupportedOperationException
        case t2: BooleanType => ???
        case _ => NullType()

      }
      case t1: BooleanType => type2 match {
        case t2: IntType => IntType()
        case t2: FloatType => IntType()
        case t2: ArrayType => ???
        case t2: BooleanType => IntType()
        case _=> NullType()

      }
    }
  }

  def mergeType(ids:List[String]):BasicType = {
    val idList = ids.toList
    var cur = getType(idList.head)
    for(id<-ids){
      cur  = mergeType(getType(id),cur)
    }

    cur
  }


  def getType(id:String):BasicType={
    if (vars contains id) {
      vars(id)
    } else {
      vars += (id -> null)
      vars(id)
    }

  }
  //  def getType(arrRef:ArrayRefExpr):BasicType={
  //    val owner=   
  //    if(vars contains owner ){
  //      vars(owner)
  //    } else {
  //      vars += (id -> null)
  //      vars(id)
  //    }
  //  }
  def getType(expr:Expr):BasicType={
    expr match {
      case e: ConstLiteralExpr=>
        e.value match {
          case e1:Int => IntType()
          case e1 : Double => DoubleType()
        }
      case e: IdExpr => getType(e.idName.id)
      case e: ArrayRefExpr => getType(e.owner.asInstanceOf[IdExpr].idName.id)
      case e: ArrayCompositionExpr => {
        val rank = getRank(e.exprs)
        val sizes  = getSize(e.exprs)
        val temp_type = getType(e)

        if(arrIsValid(ArrayType(temp_type,rank,sizes)))
          getType(e.exprs.head)
        else return NullType()
      }
      case e: FunctionCallExpr => getType(e.funcNameExpr.asInstanceOf[IdExpr].idName.id)
      case _  => NullType()
    }
  }

  def getIds(exprs:List[Expr]):List[String] ={
    var ids  =List("")
    /* check whether all elements in rhs is a id */
    val temp = exprs.map(expr=>expr.isInstanceOf[IdExpr])
    var notAllId = false
    val loop = new Breaks;
    loop.breakable{
      for(each<- temp){
        if (!each){
          notAllId = true
          loop.break()
        }

      }
    }
    /* end check*/
    if(!notAllId)
    {
      ids = exprs match {
        case e: List[IdExpr] => e.map(id=>id.idName.id)
      }
    }

    ids
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


    //if(!rhsVisitor.exprType.isInstanceOf[NullType])
    val isDeclared = ErrHelper(typer).isDeclared(stmt.rhsExpr)
    typer.updateType(id, rhsVisitor.exprType)
//    if(isDeclared){
//
//    }

    if(stmt.rhsExpr.isInstanceOf[NAryExpr]){
      val rhs_ids = stmt.rhsExpr match{
        case e: NAryExpr => typer.getIds(e.terms)

      }


      if(rhs_ids!= null && rhs_ids!=List("") ){
        val merged_rhs_ids_type = typer.mergeType(rhs_ids)
        typer.updateType(id,merged_rhs_ids_type)
      } }

    //val exprType = typer.mergeType(lhsVisitor.exprType, rhsVisitor.exprType)

    //vars += (lhs -> exprType)

    return StatementVisitor.Continue
  }
}

private class TypeExprVisitor(typer: Typer) extends ExpressionVisitor {

  val defaultType = IntType()
  var exprType: BasicType = null



//  def getType(exprs:List[Expr]):BasicType={
//    //    val newtype = exprs match {
//    //      case e: List[ConstLiteralExpr]=>e.map(each => each.kind).head
//    //      case e: List[ArrayCompositionExpr]=> getType(e.head.exprs)
//    //      case e: List[ArrayRefExpr] => getType(e.head.indices)
//    //      case e: List[IdExpr] =>e.map(each=>typer.getType(e.head.idName.id) ).head
//    //      case _ => defaultType
//    //    }
//    val newtype = exprs.map {
//      case e: ConstLiteralExpr => e.kind
//      case e: ArrayCompositionExpr => getType(e.exprs)
//      case e: ArrayRefExpr => getType(e.indices)
//      case e: IdExpr =>  typer.getType(e.idName.id)
//      case _ => defaultType
//    }
//    newtype.head
//  }

  override def visit(expr: IdExpr): Int = {
        if (typer.vars contains expr.idName.id) {
          val idType = typer.vars(expr.idName.id)
          exprType = typer.mergeType(exprType, idType)
        } else {
          typer.vars += (expr.idName.id -> null)
        }
   // no type information
    exprType = typer.updateType(expr.idName.id, defaultType)
    ErrHelper(typer).isDeclared(expr)
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
      case t: BooleanType => exprType = typer.mergeType(exprType, t)
      case t: StringType  => throw new UnsupportedOperationException
      case _ => println(expr.kind); throw new UnsupportedOperationException
    }

    return ExpressionVisitor.Continue
  }

  override def visit(expr: ArrayRefExpr): Int = {
    // we cannot determine the sizes from accessing a sub-array
    // array type is default to float
    //    typer.isDeclared(expr)


    exprType = typer.mergeType(exprType, ArrayType(defaultType, expr.rank, List.empty))

    return ExpressionVisitor.Continue
  }

  override def visit(expr: ArrayCompositionExpr): Int = {
    val rank = typer.getRank(expr.exprs)
    val sizes  = typer.getSize(expr.exprs)
    val temp_type = typer.getType(expr)
    //     val new_rank  = expr.exprs
    //     sizes.+:(expr.exprs.size)
    //    expr.exprs.foreach(xpr=>{
    ////      typer.isDeclared(xpr)
    //    })
    exprType = typer.mergeType(exprType, ArrayType(temp_type,rank, sizes))
    return ExpressionVisitor.Skip
    //  return ExpressionVisitor.Continue
  }

}