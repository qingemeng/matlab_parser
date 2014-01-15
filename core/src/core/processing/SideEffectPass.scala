package   core.processing

import   model._
import   model.statement._
import   model.expression._

import scala.collection.mutable.ListBuffer

// TODO: review code containing loop and implement based on CFG analysis

// Removes side effects e.g. i++, ++i, expression list from statements and conditions
// Prefix and postfix ++/-- are changed to +=/-= equivalents
// Statements are added using ExpressionStatement node
// * Must be preceded by StatementBlockPass
// * Must precede AssignStatementPass
class SideEffectPass extends StatementVisitor {

  private def addSideEffectStatements(inBlock: StatementBlock, postfixExprs: List[Expr]) {
    if (!postfixExprs.isEmpty) {
      for (e <- postfixExprs.reverse) {
        val s = ExpressionStatement(e)
        inBlock.prependStatement(s)
      }
    }
  }
  
  private def addSideEffectStatements(refStmt: Statement, prefixExprs: List[Expr], postfixExprs: List[Expr]) {
    if (!refStmt.getParent.isInstanceOf[StatementBlock]) 
      throw new UnsupportedOperationException(refStmt.getParent.toString)
    val blk = refStmt.getParent.asInstanceOf[StatementBlock]
    
    if (!prefixExprs.isEmpty) {
      for (e <- prefixExprs) {
        val s = ExpressionStatement(e)
        blk.insertBefore(refStmt, s)
      }
    }

    if (!postfixExprs.isEmpty) {
      for (e <- postfixExprs) {
        val s = ExpressionStatement(e)
        blk.insertAfter(refStmt, s)
      }
    }
  }
  
  override def visit(stmt: AssignmentStatement): Int = {
    throw new UnsupportedOperationException
  }
  
  override def visit(stmt: ExpressionStatement): Int = {
    val visitor = new SideEffectExprVisitor()
    var expr = ExpressionProcessor.process(stmt.expr, visitor)
    var prefixExprs = visitor.getPrefixExprs
    var postfixExprs = visitor.getPostfixExprs
    stmt.update(expr = expr)
    addSideEffectStatements(stmt, prefixExprs, postfixExprs)
    
    return StatementVisitor.Continue
  }
  
  override def visit(stmt: DeclarationStatement): Int = {
    val visitor = new SideEffectExprVisitor()
    for (d <- stmt.decls) {
      if (d.initializer.isDefined) {
    	val expr = ExpressionProcessor.process(d.initializer.get, visitor)
        d.update(initializer = Some(expr))
      }
    }

    val prefixExprs = visitor.getPrefixExprs
    val postfixExprs = visitor.getPostfixExprs
    addSideEffectStatements(stmt, prefixExprs, postfixExprs)
    
    return StatementVisitor.Continue
  }
  
  override def visit(stmt: IfStatement): Int = {
    val visitor = new SideEffectExprVisitor()
    val condExpr = ExpressionProcessor.process(stmt.condExpr, visitor)
    val prefixExprs = visitor.getPrefixExprs
    val postfixExprs = visitor.getPostfixExprs
    
    stmt.update(condExpr = condExpr)
    addSideEffectStatements(stmt, prefixExprs, List.empty)
    // postfix expressions are inserted simultaneously into then and else body
    addSideEffectStatements(stmt.thenBody.asInstanceOf[StatementBlock], postfixExprs)
    if (stmt.elseBody.isDefined) 
      addSideEffectStatements(stmt.elseBody.get.asInstanceOf[StatementBlock], postfixExprs)
    
    return StatementVisitor.Continue
  }
  
  private def visitStatementWithBody(stmt: Statement, expr: Expr, block: StatementBlock): Expr = {
    val visitor = new SideEffectExprVisitor()
    val xpr = ExpressionProcessor.process(expr, visitor)
    val prefixExprs = visitor.getPrefixExprs
    val postfixExprs = visitor.getPostfixExprs
    addSideEffectStatements(stmt, prefixExprs, List.empty)
    addSideEffectStatements(block, postfixExprs)
    return xpr
  }
  override def visit(stmt: WhileStatement): Int = {
    // XXX: incorrect for prefix ops
    val condExpr = visitStatementWithBody(stmt, stmt.condExpr, stmt.body.asInstanceOf[StatementBlock])
    stmt.update(condExpr = condExpr)
    return StatementVisitor.Continue
  }
  override def visit(stmt: DoStatement): Int = {
    // XXX: incorrect for postfix ops
    val condExpr = visitStatementWithBody(stmt, stmt.condExpr, stmt.body.asInstanceOf[StatementBlock])
    stmt.update(condExpr = condExpr)
    return StatementVisitor.Continue
  }
  override def visit(stmt: SwitchStatement): Int = {
    val condExpr = visitStatementWithBody(stmt, stmt.condExpr, stmt.body.asInstanceOf[StatementBlock])
    stmt.update(condExpr = condExpr)
    return StatementVisitor.Continue
  }

  override def visit(stmt: ReturnStatement): Int = {
    stmt.returnValue match {
      case Some(expr) => {
        val visitor = new SideEffectExprVisitor()
        val returnValue = ExpressionProcessor.process(expr, visitor)
        val prefixExprs = visitor.getPrefixExprs
        val postfixExprs = visitor.getPostfixExprs
        if (!postfixExprs.isEmpty) throw new UnsupportedOperationException
        if(returnValue != null)
          stmt.update(Some(returnValue))
        addSideEffectStatements(stmt, prefixExprs, List.empty)
      }
      case None =>
    }
    return StatementVisitor.Continue
  }
  
  private def handleExprList(exprs: List[Expr]): List[Expr] = {
    // handle side effects and order the expressions
    val visitor = new SideEffectExprVisitor()
    var procExprs = ListBuffer.empty[Expr]
    for (xpr <- exprs) {
      val retXpr = ExpressionProcessor.process(xpr, visitor)
      procExprs ++= visitor.getPrefixExprs
      if (!NullStatementPass.isNullEffectExpr(retXpr)) procExprs += retXpr
      procExprs ++= visitor.getPostfixExprs
      visitor.reset()
    }
      
    procExprs.toList
  }
  
  // Need to handle cases like these:
  // initStmt may be int i=0, k=i
  // iterExpr may be i++, k++
  override def visit(stmt: ForStatement): Int = {
    // get the induction variable from lhs
    var inductionVar = stmt.condExpr match {
      case Some(xpr) => xpr match {
        case e: NAryExpr => e.terms.head match {
          case e: IdExpr => e.idName
          case other     => null
        }
        case _ => null
      }
      case None => null
    }
    
    if (inductionVar == null) return StatementVisitor.Continue
    
    if (stmt.initStmt!=null) {
      val initExprs = stmt.initStmt match {
        case s: DeclarationStatement => 
          s.decls.filter(d => d.initializer != None).map(d => NAryExpr(OpTempAssign(), List(IdExpr(d.idName), d.initializer.get)))
        case s: ExpressionStatement  => s.expr match {
          case e: ExpressionListExpr => e.exprs
          case other => List(other)
        }
      }
      
      val procExprs = handleExprList(initExprs)
      
      // XXX: should handle code motion with dependencies, i.e. move induction variable expression to last
      // For now just handle the case where last expression is the initialization of induction variable
      var hasLastInit = false
      procExprs.last match {
        case xpr: NAryExpr => xpr.terms.head match {
          case e: IdExpr => if (e.idName == inductionVar) hasLastInit = true
          case _ =>
        }
        case _ =>
      }
      if (hasLastInit) {
        stmt.update(initStmt = ExpressionStatement(procExprs.last))
        addSideEffectStatements(stmt, procExprs.dropRight(1).toList, List.empty)
      } else {
        stmt.update(initStmt = null)
        addSideEffectStatements(stmt, procExprs.toList, List.empty)
      }
    }

    if (stmt.iterExpr.isDefined) {
      // XXX: not handled now
      /*
      val iterExprs = stmt.iterExpr.get match {
        case e: ExpressionListExpr => e.exprs
        case other => List(other)
      }
      
      val procExprs = handleExprList(iterExprs)
      */
      // XXX: if there are more than one expression, we must place them in every exit branch
    }
    return StatementVisitor.Continue
  }
}

object SideEffectPass {
  def doPass(tu: TranslationUnit): Unit = {
    StatementProcessor.process(tu, new SideEffectPass())
  }
  def doPass(function: FunctionDef): Unit = {
    StatementProcessor.process(function, new SideEffectPass())
  }
}


// Side-effects expression visitor
private class SideEffectExprVisitor extends ExpressionVisitor {

  private var _prefix = ListBuffer.empty[Expr]
  private var _postfix = ListBuffer.empty[Expr]
  
  def getPrefixExprs = _prefix.toList
  def getPostfixExprs = _postfix.toList
  
  def reset() {
    _prefix.clear()
    _postfix.clear()
  }
  // change unary prefix and postfix operators to binary operators
  // e.g. i++ or ++i -> i+=1 
  override def leave(expr: UnaryExpr): Expr = {
    def makeExpr(op: NAryOp, xpr: Expr): Expr = {
      assert(xpr.isLValue)
      NAryExpr(op, List(xpr.cloneExpr, ConstLiteralExpr(1)))
    }
    def extractPrefix(op: NAryOp, xpr: UnaryExpr): Expr = {
      _prefix += makeExpr(op, xpr.term)
      xpr.term
    }
    def extractPostfix(op: NAryOp, xpr: UnaryExpr): Expr = {
      _postfix += makeExpr(op, xpr.term)
      xpr.term
    }
    expr.op match {
      case OpPrefixInc()  => extractPrefix(OpTempPlusAssign(), expr)
      case OpPrefixDec()  => extractPrefix(OpTempMinusAssign(), expr)
      case OpPostfixInc() => extractPostfix(OpTempPlusAssign(), expr)
      case OpPostfixDec() => extractPostfix(OpTempMinusAssign(), expr)
      case _              => expr
    }
  }
  
  override def leave(expr: NAryExpr): Expr = {
    def extractPrefix(xpr: NAryExpr): Expr = {
      if(xpr.terms(0).isInstanceOf[FunctionCallExpr]){ return xpr } //check if it is DSL
      _prefix += xpr.cloneExpr
      xpr.terms(0)
    }
    expr.op match {
      case OpTempAssign() => extractPrefix(expr)
      case OpTempPlusAssign()       => extractPrefix(expr)
      case OpTempMinusAssign()      => extractPrefix(expr)
      case OpTempTimesAssign()      => extractPrefix(expr)
      case OpTempDivideAssign()     => extractPrefix(expr)
      case OpTempModuloAssign()     => extractPrefix(expr)
      case OpTempShiftLeftAssign()  => extractPrefix(expr)
      case OpTempShiftRightAssign() => extractPrefix(expr)
      case OpTempBinaryAndAssign()  => extractPrefix(expr)
      case OpTempBinaryXorAssign()  => extractPrefix(expr)
      case OpTempBinaryOrAssign()   => extractPrefix(expr)
      case _                        => expr
    }
  }
  
  override def leave(expr: ExpressionListExpr): Expr = {
    val last = expr.exprs.last
    val rest = expr.exprs.dropRight(1)
    _prefix ++= rest
    last
  }
}