package core.util

import model.expression._
import scala.collection.mutable
import model.{NullType, ArrayType, BasicType}
import refactoring.matlab.processing.Typer

/**
 * Created with IntelliJ IDEA.
 * User: gemengqin
 * Date: 3/19/14
 * Time: 12:11 PM
 * To change this template use File | Settings | File Templates.
 */
object ErrHelper {

  def apply(typer:Typer) =  new ErrHelper(typer)


}

class ErrHelper(_typer:Typer){
  val typer = _typer




  def getErrMsg(errId:Int, line: Int) :String ={

    errId match {
      case ErrConstants.NullValue => "null value found in line: "  +line
      case ErrConstants.VarNotInitialized => " is not declared"  + " in line: " +  line
      case ErrConstants.ArrContainsVariousSubType => " Array elements has different data types in line: "  +line
      case ErrConstants.ArrSizeNotMatch=> " Array sizes don't match in line: " +line
      case ErrConstants.ArrSubTypeNotMatch=> " Array sub type don't match in line: " +line

    }



  }
  private def isDeclared(rhsId:String,line:Int):Boolean  ={

    if(!(typer.vars contains rhsId)){
      println(ErrConstants.ErrPrefix + rhsId + " " + getErrMsg(ErrConstants.VarNotInitialized, line))
      return false
    }
    return true

  }
  def isDeclared(rhsExpr:Expr) :Boolean= rhsExpr match {
    case e: IdExpr => isDeclared(e.idName.id, rhsExpr.pos.line)
    case e: ArrayRefExpr => isDeclared(e.owner.asInstanceOf[IdExpr].idName.id,rhsExpr.pos.line)
    case e: ArrayCompositionExpr => isDeclared(e)
    case e: FunctionCallExpr=> isDeclared(e.funcNameExpr.asInstanceOf[IdExpr].idName.id,rhsExpr.pos.line)
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
  def arrIsValid(arrType:ArrayType):Boolean  = {
    val line = arrType.sizes.head.pos.line
    val sizes = arrType.sizes
    val headType = typer.getType(sizes.head)

    if (sizes==null)
      println(ErrConstants.ErrPrefix+ getErrMsg(ErrConstants.NullValue,line))
      return false

    for(size<-sizes){
      val exp_type = typer.getType(size)
      if(exp_type==null || exp_type== NullType() )
        println(ErrConstants.ErrPrefix+ getErrMsg(ErrConstants.NullValue,line))
        return false
      if(exp_type!=headType)
        println(ErrConstants.ErrPrefix+ getErrMsg(ErrConstants.NullValue,line))
        return false
    }
    val arrHead = sizes.head
    if(arrHead.isInstanceOf[ArrayCompositionExpr]){

      val rank = typer.getRank(arrHead.asInstanceOf[ArrayCompositionExpr].exprs)
      val sizes  = typer.getSize(arrHead.asInstanceOf[ArrayCompositionExpr].exprs)
      if(!arrIsValid(ArrayType(headType,rank ,sizes))){
        return false
      }
    }

    return true


  }
  def canBeMerged(t1: ArrayType, t2: ArrayType): Boolean = {
    val line = t1.sizes.head.pos.line
    if(t1.rank!=t2.rank)
    {
      println(ErrConstants.ErrPrefix+ getErrMsg(ErrConstants.ArrSizeNotMatch,line))
      return false
    }
    else if(!arrIsValid(t1) ){
      return false
    }
    else if(!arrIsValid(t2)){
      return false
    }
    else if (t1.subType!=t2.subType ) {

      println(ErrConstants.ErrPrefix+ getErrMsg(ErrConstants.ArrSubTypeNotMatch,line))
      return false
    }

    else if(t1.sizes.length!=t2.sizes.length){

      println(ErrConstants.ErrPrefix+ getErrMsg(ErrConstants.ArrSizeNotMatch,line))
      return false
    }
    else if(t1.subType.isInstanceOf[ArrayType]){
      if(!canBeMerged(t1.subType.asInstanceOf[ArrayType],t2.subType.asInstanceOf[ArrayType]))
        return false
    }
    return true

  }
}
