package   core.processing

import   core._
import   model._
import   model.expression._
import org.eclipse.cdt.core.dom.ast.IVariable
import org.eclipse.cdt.core.dom.ast.IType
import org.eclipse.cdt.core.dom.ast.IBasicType
import org.eclipse.cdt.core.dom.ast.IArrayType
import org.eclipse.cdt.core.dom.ast.IPointerType
import org.eclipse.cdt.core.dom.ast.IBinding


import scala.collection.mutable.ListBuffer

object ArrayHelper {
  
  // extracts IdExpr(s) from all array indices
  def extractIdExprs(array: ArrayRefExpr): List[IdExpr] = {
    var ids = List.empty[IdExpr]

    array.indices.foreach(index => {
      ExpressionProcessor.process(index, new ExpressionVisitor(){
        override def visit(expr: IdExpr): Int = {
          ids = expr :: ids
          ExpressionVisitor.Continue
        }
      })
    })
      
    ids.reverse
  }
  
  // replace induction variable in array index
  def replaceInductionVar(array: ArrayRefExpr, idName: IdName, expr: Expr): Unit = {
    val indices = array.indices.map(index => {
      ExpressionProcessor.process(index, new ExpressionVisitor(){
        override def leave(e: IdExpr): Expr = {
          if (e.idName == idName) expr else e
        }
      })
    })    
    
    array.update(indices = indices)
  }
  

  //----------------------------------------------------------------------------
  // to build in expr type in Expr class

  
  def inferArrayInfo(array: ArrayRefExpr, declUseMap: DeclUseMap): Option[ArrayInfo] = {
    var nDim = 0
    var sizes = ListBuffer.empty[Expr]
    var basicType: BasicType = null

    def deduceArrayType(vtype: IType): Unit = vtype match {
      case t: IArrayType => 
        nDim += 1
        sizes += ConstLiteralExpr(t.getSize().numericalValue().toInt)
        deduceArrayType(t.getType())
      case t: IBasicType => t.getKind match {
        case IBasicType.Kind.eInt => basicType = IntType()
        case IBasicType.Kind.eFloat => basicType = FloatType()
        case IBasicType.Kind.eDouble => basicType = DoubleType()
      }
    }
    
    def deducePointerType(vtype: IType): Unit = vtype match {
      // for pointer type, we don't know if it is 1D or 2D
      case t: IPointerType =>
        nDim += 1
        sizes += ConstLiteralExpr(-1) // dynamic size
        deducePointerType(t.getType())
      case t: IBasicType => t.getKind match {
        case IBasicType.Kind.eInt => basicType = IntType()
        case IBasicType.Kind.eFloat => basicType = FloatType()
        case IBasicType.Kind.eDouble => basicType = DoubleType()
      }
    }

    // find idname for array
    val idName = array.owner match {
      case e: IdExpr       => e.idName
      case e: FieldRefExpr => e.fieldName
      case other           => null
    }
    if (idName == null) None
    else {
      val declUseGroup = declUseMap.getDeclUse(idName)
      // array can either be fixed size or dynamically declared
//      val declNames = getAst().getDefinitions(binding)
      val variable = declUseGroup.declNames(0).resolveBinding().asInstanceOf[IVariable]
      val varType = variable.getType
      varType match {
        case t: IArrayType   => deduceArrayType(varType)
        case t: IPointerType => deducePointerType(varType)
      }
    }
    
    Some(ArrayInfo(basicType, nDim, sizes.toList))
  }

}

