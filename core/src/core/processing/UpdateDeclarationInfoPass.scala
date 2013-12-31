package   core.processing

import   core._
import   model._
import   model.statement._
import   model.expression._

// Updates DeclUseMap
//
// * Should be preceded by ArrayDimensionPass and PragmaArrayPass
// * Can be used as the last pass to update DeclUseMap

class UpdateDeclarationInfoPass(declUseMap: DeclUseMap) extends StatementVisitor {
  override def leave(stmt: AssignmentStatement): Unit = {
    if(stmt.lhsExpr.isInstanceOf[IdExpr] && stmt.rhsExpr.isInstanceOf[AllocateArrayExpr]){
      val idExpr = stmt.lhsExpr.asInstanceOf[IdExpr]
      val declUseGroup = declUseMap.getDeclUse(idExpr.idName)
      
      //Update only if array and the sizes are unknown
      if(declUseGroup.isArray && declUseGroup.getArrayInfo.hasUnknownSize()){
          val arrayInfo = stmt.rhsExpr.asInstanceOf[AllocateArrayExpr].arrayInfo
    	  declUseGroup.updateArrayInfo(arrayInfo.cloneArrayInfo())
      }
      
      //println("Updated: " + PrettyPrinter.pretty(stmt.rhsExpr))
    }
  }
  
  override def leave(stmt: DeclarationStatement): Unit = {
    stmt.decls.foreach(decl => {
      val declUseGroup = declUseMap.getDeclUse(decl.idName)
      
      //Update only if array and the sizes are unknown
      if(declUseGroup.isArray && declUseGroup.getArrayInfo.hasUnknownSize()){
	      decl.initializer match {
	        case Some(expr) => if(expr.isInstanceOf[AllocateArrayExpr]){
	          val arrayInfo = expr.asInstanceOf[AllocateArrayExpr].arrayInfo
	          declUseGroup.updateArrayInfo(arrayInfo.cloneArrayInfo())
	          //println("Updated: " + PrettyPrinter.pretty(expr))
	        }
	        case None =>
	      }
      }
    })
  }
}

object UpdateDeclarationInfoPass {
  def doPass(tu: TranslationUnit, declUseMap: DeclUseMap): Unit = {
    StatementProcessor.process(tu, new UpdateDeclarationInfoPass(declUseMap))
  }
  def doPass(func: FunctionDef, declUseMap: DeclUseMap): Unit = {
    StatementProcessor.process(func, new UpdateDeclarationInfoPass(declUseMap))
  }
}