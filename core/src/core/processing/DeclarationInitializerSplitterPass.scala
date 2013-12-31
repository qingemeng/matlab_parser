package   core.processing

import   core._
import   model._
import   model.statement._
import   model.expression._

//Split the initializers from the declarations
class DeclarationInitializerSplitterPass(declUseMap: DeclUseMap) extends StatementVisitor{
	override def leave(stmt: DeclarationStatement): Unit = {
	  val parent = stmt.getParent.asInstanceOf[StatementBlock]
	  
	  stmt.decls.foreach(decl => {
	    decl.initializer match {
	      case Some(expr) => {
		    val idName = decl.idName
		    
		    val declUseGroup = declUseMap.getDeclUse(idName)
		    val lhs = if(declUseGroup.isArray){
		      ArrayRefExpr(IdExpr(idName), declUseGroup.getArrayInfo.sizes)
		    } else {
		      IdExpr(idName)
		    }
		    
		    //Insert assignment
		    val newStmt = AssignmentStatement(lhs, expr, OpAssign())
		    parent.insertAfter(stmt, newStmt)
		    
		    //Remove initializer
		    decl.update(None)
	      }
	      case None =>
	    }
	  })
	}
}

object DeclarationInitializerSplitterPass {
  def doPass(tu: TranslationUnit, declUseMap: DeclUseMap): Unit = {
    StatementProcessor.process(tu, new DeclarationInitializerSplitterPass(declUseMap))
  }
  def doPass(function: FunctionDef, declUseMap: DeclUseMap): Unit = {
    StatementProcessor.process(function, new DeclarationInitializerSplitterPass(declUseMap))
  }
}