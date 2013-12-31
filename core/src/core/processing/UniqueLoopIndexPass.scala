package   core.processing

import scala.collection.mutable.HashSet

import   model._
import   model.statement._
import   model.expression._

//Changes all the indices to unique indices for "ForStatement". Does not update the DeclUseMap
class UniqueLoopIndexPass extends StatementVisitor{
	val indices:HashSet[String] = HashSet()
	
	override def visit(stmt: ForStatement): Int = {
	  LoopHelper.hasValidForHeader(stmt) map {
	    case loopInfo => {
  	    val currIndex = loopInfo.inductionVar.name
       	var i = 0
          while(indices.contains(currIndex + "_" + i)){
      	  i = i + 1
      	}
  	    val newIndex = currIndex  + "_" + i
  	    indices += newIndex
  
  	    val visitor = new RenameLoopIndex(currIndex, newIndex)
  	    ExpressionProcessor.process(stmt, visitor)
	    }
	  }
	  StatementVisitor.Continue
	}
	
	class RenameLoopIndex(val oldIndex:String, val newIndex:String) extends ExpressionVisitor {
	  override def leave(expr: IdExpr): Expr = {
	    if(expr.idName.name == oldIndex){
	      IdExpr(IdName(newIndex, newIndex))
	    } else {
	      expr
	    }
	  }
	}
}

object UniqueLoopIndexPass {
  def doPass(func: FunctionDef){
    StatementProcessor.process(func, new UniqueLoopIndexPass)
  }
  
  def doPass(stmt: Statement){
    StatementProcessor.process(stmt, new UniqueLoopIndexPass)
  }
}

