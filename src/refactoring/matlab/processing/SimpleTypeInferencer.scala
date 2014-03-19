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
private class Typer {
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
  private def isDeclared(rhsId:String,line:Int):Boolean  ={

    if(!(vars contains rhsId)){
      println(ErrConstants.ErrPrefix + rhsId + " " + ErrMsgs.getErrMsg(ErrConstants.VarNotInitialized, line))
      return false
    }
    return true

  }

  def isDeclared(rhsExpr:Expr) :Boolean= rhsExpr match {
    case e: IdExpr => isDeclared(e.idName.id, rhsExpr.pos.line)
    case e: ArrayRefExpr => isDeclared(e.owner.asInstanceOf[IdExpr].idName.id,rhsExpr.pos.line)
    case e: ArrayCompositionExpr => isDeclared(e)
    case e: NAryExpr => {
      val areDeclared  = e.terms.map(each => isDeclared(each))

      if(areDeclared.contains(false) ){
        return false
      }
      return true

    }

    case _ => true
    //TODO:check if the function call is declared
    //    case e: FunctionCallExpr => isDeclaration()
  }

  //  def getArrType(expr:ArrayCompositionExpr): BasicType = {
  //    expr.exprs.map(xpr => if(xpr.isInstanceOf[ArrayCompositionExpr]){
  //      getArrType(xpr)
  //    }else{
  //      expr.exprs.map(xpr => xp)
  //    }
  //    )
  //
  //  }

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
    typer.isDeclared(stmt.rhsExpr)
    typer.updateType(id, rhsVisitor.exprType)

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

  val defaultType = FloatType()
  var exprType: BasicType = null

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

  def getType(exprs:List[Expr]):BasicType={
    //    val newtype = exprs match {
    //      case e: List[ConstLiteralExpr]=>e.map(each => each.kind).head
    //      case e: List[ArrayCompositionExpr]=> getType(e.head.exprs)
    //      case e: List[ArrayRefExpr] => getType(e.head.indices)
    //      case e: List[IdExpr] =>e.map(each=>typer.getType(e.head.idName.id) ).head
    //      case _ => defaultType
    //    }
    val newtype = exprs.map {
      case e: ConstLiteralExpr => e.kind
      case e: ArrayCompositionExpr => getType(e.exprs)
      case e: ArrayRefExpr => getType(e.indices)
      case e: IdExpr =>  typer.getType(e.idName.id)
      case _ => defaultType
    }
    newtype.head
  }

  override def visit(expr: IdExpr): Int = {
    //    if (vars contains expr.idName.id) {
    //      val idType = vars(expr.idName.id)
    //      exprType = SimpleTypeInferencer.mergeType(exprType, idType)
    //    } else {
    //      vars += (expr.idName.id -> null)
    //    }
    // no type information
    //exprType = typer.updateType(expr.idName.id, defaultType)
//    typer.isDeclared(expr)
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
    val rank = getRank(expr.exprs)
    val sizes  = getSize(expr.exprs)
    val temp_type = getType(expr.exprs)
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