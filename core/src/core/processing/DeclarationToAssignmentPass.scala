package   core.processing

import   model._
import   model.expression._
import   model.statement._

//Convert Declaration Statements to Assignments
class DeclarationToAssignmentPass extends StatementVisitor {
  override def leave(stmt: DeclarationStatement) {
    val parent = stmt.getParent.asInstanceOf[StatementBlock]
    stmt.decls.foreach(decl => {
      val initializer = decl.initializer match {
        //0 is used as sentinel for uninitialized decls
        //case Some(x: AllocateArrayExpr) => ConstLiteralExpr(0)
        case Some(x)                    => x
        case None                       => ConstLiteralExpr(0)
      }

      val assignment = AssignmentStatement(IdExpr(decl.idName), initializer, OpAssign())
      parent.insertBefore(stmt, assignment)
    })
    parent.removeStatement(stmt)
  }
}

object DeclarationToAssignmentPass {
  def doPass(tu: TranslationUnit){
    StatementProcessor.process(tu, new DeclarationToAssignmentPass)
  }
  def doPass(func: FunctionDef){
    StatementProcessor.process(func, new DeclarationToAssignmentPass)
  }
}