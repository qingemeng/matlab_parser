package   core.processing

import   core._
import   model._
import   model.statement._
import   model.expression._
import   core.processing._
import   model.property._

// Identifies countable loops that can be refactored, i.e.
//  - countable, currently for-loops with proper start, end and stride
//  - no control dependent flow,
//  - no while, do, if, switch, conditional-expr, break, continue, return
//  - all statements in loop must be AssignmentStatement
//  - FunctionCallExpr in statement assumed no side-effect and single-valued
//
// * Must be preceded by StatementBlockPass
// * Must be preceded by AssignmentStatementPass
// * Must be preceded by ExpressionNormalizationPass
class LoopSplittingPass() extends StatementVisitor {
  private val DEBUG_PRINT = false
  private val SPLIT = true

  private def hasNoInvalidStatements(stmt: ForStatement, loopInfo: LoopInfo): Boolean = {
    // XXX: For now, inner loops considered to fail the test
    def testStatement(stmt: Statement): Boolean = stmt match {
      case s: StatementBlock        => s.statements.forall(s1 => testStatement(s1))
      case s: FunctionCallStatement => false
      case s: DeclarationStatement  => false
      case s: ExpressionStatement   => false
      case s: ForStatement          => false //s.getCanAbstractLoop
      case s: IfStatement           => false
      case s: WhileStatement        => false //s.getCanAbstractLoop
      case s: DoStatement           => false //s.getCanAbstractLoop
      case s: SwitchStatement       => false
      case s: CaseStatement         => false
      case s: ReturnStatement       => false
      case s: BreakStatement        => false
      case s: ContinueStatement     => false
      case s: NullStatement         => false
      case s: GotoStatement         => false
      case s: LabelStatement        => false
      case s: AssignmentStatement   => testDiagonalIndexing(s)
    }
    // XXX: for now array diagonal indexing not supported
    def testDiagonalIndexing(stmt: AssignmentStatement): Boolean = {
      var pass = true
      val visitor = new ExpressionVisitor() {
	    override def leave(expr: ArrayRefExpr): Expr = {
	      if(loopInfo==null)
	      {
				pass=false
				return expr
	      }
	      var idNames = ArrayHelper.extractIdExprs(expr).filter(id => id.idName == loopInfo.inductionVar).map(id => id.idName)
	      // error if at least two of the same index
	      if (idNames.distinct.size != idNames.size)
	        pass = false
	      expr
	    }
	  }

      ExpressionProcessor.process(stmt.lhsExpr, visitor)
      ExpressionProcessor.process(stmt.rhsExpr, visitor)
      pass
    }

    val blk = stmt.body.asInstanceOf[StatementBlock]
    blk.statements.forall(testStatement)
  }

  // Checks that domain is rectangular
  // Goes through each child statement to ensure that child statement domain does not depend on parent
  private def hasValidDomains(loopInfo: LoopInfo, stmt: ForStatement): Boolean = {
    def containsInductionVar(expr: Expr): Boolean = {
      var found = false
      ExpressionProcessor.process(expr, new ExpressionVisitor() {
        override def visit(idExpr: IdExpr): Int = {
        	if(loopInfo==null)
			{
				return ExpressionVisitor.Continue
			}
          if (idExpr.idName == loopInfo.inductionVar) {
            found = true
            return ExpressionVisitor.Abort
          }
          return ExpressionVisitor.Continue
        }
      })

      return found
    }
    // XXX: For now, check induction variable not contained
    def testDomain(stmt: Statement): Boolean = {
      if (stmt.isInstanceOf[AssignmentStatement]) {
        val s = stmt.asInstanceOf[AssignmentStatement]
        s.props[DomainsProperty].getDomains.forall(l =>
          !(containsInductionVar(l.lowerBound) || containsInductionVar(l.upperBound) || containsInductionVar(l.stride))
        )
      } else {
        false
      }
    }

    val blk = stmt.body.asInstanceOf[StatementBlock]
    val isValid = blk.statements.forall(testDomain)
    isValid
  }

  override def leave(stmt: ForStatement): Unit = {
    LoopHelper.hasValidForHeader(stmt) match {
      case Some(loopInfo) => {
        val validStmts = hasNoInvalidStatements(stmt, loopInfo)
        val validDomains = hasValidDomains(loopInfo, stmt)

        // Check if loop body has valid structure
        val valid = validStmts && validDomains

        if (DEBUG_PRINT) {
          if (!valid) println("x " + stmt.pretty())
          else println("/ " + stmt.pretty())
        }

        if (!valid) return

        // XXX: Assumes no aliasing
        // Note locality is lost after splitting
        if (SPLIT) {
          val blk = stmt.body.asInstanceOf[StatementBlock]

          // Build dependency graph
          // 'sorted' contains list of blocks of statements
          val stmts = blk.statements.map(s => s.asInstanceOf[AssignmentStatement])
          val graph = DependencyGraph.buildSCC(stmts, loopInfo.inductionVar)
          val sorted = DependencyGraph.topoSort(stmts, graph)
          //DependencyGraph.printGraph(stmts, graph)

          // XXX: Currently loop splitting is implemented as all or none
          if ((sorted.size == stmts.size) && sorted.forall(l => l.size == 1)) {
          stmt.props[LoopProperty].setCanAbstractLoop(true)

            blk.statements.foreach(s => s.props[DomainsProperty].prependDomain(loopInfo))
          val parentBlk = stmt.getParent.asInstanceOf[StatementBlock]
          for (s <- blk.statements) {
            parentBlk.insertBefore(stmt, s)
            //if (stmtChangeCollector != null) stmtChangeCollector.insertStmtBefore(parentBlk, stmt, s)
          }
          // Remove 'for' block
          parentBlk.removeStatement(stmt)
          //if (stmtChangeCollector != null) stmtChangeCollector.removeStmt(parentBlk, stmt)
          }
          /*// Original code assuming all statements are liftable
          for (s <- blk.statements) {
            parentBlk.insertBefore(stmt, s)
            if (stmtChangeCollector != null) stmtChangeCollector.insertStmtBefore(parentBlk, stmt, s)
          }
          parentBlk.removeStatement(stmt)
          if (stmtChangeCollector != null) stmtChangeCollector.removeStmt(parentBlk, stmt)
          */
        }
      }

      case None => {
        if (DEBUG_PRINT) {
          println("x " + stmt.pretty())
        }
      }
    }

  }
}

object LoopSplittingPass {
  def doPass(tu: TranslationUnit): Unit = {
    StatementProcessor.process(tu, new LoopSplittingPass())
  }
  def doPass(function: FunctionDef): Unit = {
    StatementProcessor.process(function, new LoopSplittingPass())
  }
}