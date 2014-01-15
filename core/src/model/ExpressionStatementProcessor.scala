package   model

import   model._
import   model.expression._
import   model.statement._

object ExpressionStatementProcessor {
  def apply(visitor: ExpressionVisitor) = new ExpressionStatementProcessor(new ExpressionProcessor(visitor))
  def apply(exprProc: ExpressionProcessor) = new ExpressionStatementProcessor(exprProc)
}

class ExpressionStatementProcessor(private val exprProc: ExpressionProcessor) extends StatementVisitor {
  override def leave(stmt: FunctionCallStatement): Unit = {
	val newExpr = exprProc.process(stmt.funcCallExpr)
	if(newExpr != null && newExpr.isInstanceOf[FunctionCallExpr])
		stmt.update(newExpr.asInstanceOf[FunctionCallExpr])
  }
  
  override def leave(stmt: DeclarationStatement): Unit = {
    stmt.decls.foreach(decl => {
      decl.initializer match {
        case Some(expr) => {
          val newExpr = exprProc.process(expr)
          if(newExpr != null){
            decl.update(Some(newExpr))
          }
        }
        case None =>
      }
    })
  }
  
  override def leave(stmt: ExpressionStatement): Unit = {
	val newExpr = exprProc.process(stmt.expr)
	if(newExpr != null)
		stmt.update(newExpr)
  }
  
  override def leave(stmt: ForStatement): Unit = {
    stmt.initStmt match {
      case initStmt=> initStmt match {
        case s: FunctionCallStatement => leave(s)
        case s: DeclarationStatement  => leave(s)
        case s: ExpressionStatement   => leave(s)
        case s: NullStatement         => leave(s)
        case s: AssignmentStatement   => leave(s)        
      }
      case null =>
    }
    
    stmt.condExpr match {
      case Some(expr) => {
        val newExpr = exprProc.process(expr)
          if(newExpr != null){
            stmt.update(stmt.initStmt, Some(newExpr), stmt.iterExpr, stmt.body)
          }
        }
      case None =>
    }
    
    stmt.iterExpr match {
      case Some(expr) => {
        val newExpr = exprProc.process(expr)
          if(newExpr != null){
            stmt.update(stmt.initStmt, stmt.condExpr , Some(newExpr), stmt.body)
          }
        }
      case None =>
    }
  }
  
  override def leave(stmt: IfStatement): Unit = {
	val newExpr = exprProc.process(stmt.condExpr)
	if(newExpr != null)
		stmt.update(newExpr)
  }
  
  override def leave(stmt: WhileStatement): Unit = {
	val newExpr = exprProc.process(stmt.condExpr)
	if(newExpr != null)
		stmt.update(newExpr)
  }
  
  override def leave(stmt: DoStatement): Unit = {
	val newExpr = exprProc.process(stmt.condExpr)
	if(newExpr != null)
		stmt.update(newExpr)
  }
  
  override def leave(stmt: SwitchStatement): Unit = {
	val newExpr = exprProc.process(stmt.condExpr)
	if(newExpr != null)
		stmt.update(newExpr)    
  }
  
  override def leave(stmt: ReturnStatement): Unit = {
    stmt.returnValue match {
      case Some(expr) => {
	    val newExpr = exprProc.process(expr)
	    if(newExpr != null)
		  stmt.update(Some(newExpr))
      }
      case None =>
    }
  }
  
  override def leave(stmt: AssignmentStatement): Unit = {
    val lhsExpr = exprProc.process(stmt.lhsExpr)
    val rhsExpr = exprProc.process(stmt.rhsExpr)
    
    if(lhsExpr != null)
      stmt.update(lhsExpr)
    if(rhsExpr != null)
      stmt.update(stmt.lhsExpr, rhsExpr)
  }
}