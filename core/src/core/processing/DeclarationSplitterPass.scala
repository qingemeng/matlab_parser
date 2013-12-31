package   core.processing

import   model._
import   model.statement._

//Perform this pass after refactoring to split the declaration statement
class DeclarationSplitterPass extends StatementVisitor {
  override def leave(stmt: DeclarationStatement): Unit = {
    if(stmt.decls.size > 1){
      val parent = stmt.getParent
      if(parent.isInstanceOf[StatementBlock]){
        val parentBlock = parent.asInstanceOf[StatementBlock]
        
		stmt.decls.foreach(decl => {
			val declStatement = new DeclarationStatement(List(decl))
			parentBlock.insertAfter(stmt, declStatement)
		})
        
        parentBlock.removeStatement(stmt)
      } else {
        throw new UnsupportedOperationException("Parent is not a statement block")
      }
    }
  }
}


object DeclarationSplitterPass {
  def doPass(tu: TranslationUnit){
    StatementProcessor.process(tu, new DeclarationSplitterPass)
  }
  def doPass(func: FunctionDef){
    StatementProcessor.process(func, new DeclarationSplitterPass)
  }
}