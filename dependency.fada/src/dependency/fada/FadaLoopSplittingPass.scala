package dependency.fada

import  core._
import  model._
import  model.statement._
import  model.expression._
import  model.property._
import  core.processing._

// Identifies countable loops that can be refactored, i.e.
//  - countable, currently for-loops with proper start, end and stride
//  - no control dependent flow, 
//  - no while, do, if, switch, conditional-expr, break, continue, return
//  - all statements in loop must be AssignmentStatement
//  - FunctionCallExpr in statement assumed no side-effect and single-valued
//
// * Must be preceded by StatementBlockPass
// * Must be preceded by AssignmentStatementPass
// * Must be preceded by AssignmentOnlyPass
class FadaLoopSplittingPass() extends StatementVisitor {
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
  
  override def leave(forStmt: ForStatement): Unit = {
    LoopHelper.hasValidForHeader(forStmt) match {
      case Some(loopInfo) => {
        if (DEBUG_PRINT) println("/ " + forStmt.pretty())
        
        //val validStmts = hasNoInvalidStatements(forStmt, loopInfo)
        //val validDomains = hasValidDomains(loopInfo, forStmt)        
    
        // Check if loop body has valid structure
        //val valid = validFor && validStmts && validDomains        
        
        //    println("validStmts: " + validStmts)
        //    println("validDomains: " + validDomains)
        
        // XXX: Assumes no aliasing
        // Note locality is lost after splitting
    
        // Build dependency graph
        if (SPLIT) {
          //If single child, just lift it
          if (forStmt.body.asInstanceOf[StatementBlock].statements.size <= 1) {
            //Lift original
            forStmt.cloneSrc.foreach(s => {
              val orgForStmt = s.asInstanceOf[ForStatement]
              val assignmentStmt = orgForStmt.body.asInstanceOf[StatementBlock].statements(0).asInstanceOf[AssignmentStatement]
              processOriginalForStatement(orgForStmt, List(List(assignmentStmt)))
            })
          } else {
            // 'sorted' contains list of blocks of statements
            val sorted = FadaAnalyzer.buildSCCandSort(forStmt)
    
            //Split original
            forStmt.cloneSrc.foreach(s => {
              processOriginalForStatement(s.asInstanceOf[ForStatement], remapToOriginal(sorted))
            })
    
            //Split current
            splitForStatment(forStmt, sorted)
          }
        }        
        
      }
      case None => if (DEBUG_PRINT) {
        println("x " + forStmt.pretty())
        
      } 
    }
  }

  def remapToOriginal(sorted: List[List[AssignmentStatement]]) = {
    //Map back to original statements
    val originalSorted = sorted.map(list => 
        list.filter(_.cloneSrc.isDefined)
            .map(_.cloneSrc.get.asInstanceOf[AssignmentStatement])
      ).filterNot(_.isEmpty)
      
    originalSorted
  }
  
  def splitForStatment(forStmt: ForStatement, sorted: List[List[AssignmentStatement]]){
    val blk = forStmt.body.asInstanceOf[StatementBlock]
    val parentBlk = forStmt.getParent.asInstanceOf[StatementBlock]

    sorted.foreach(stmtList => {
      val newForStmt = forStmt.cloneStmt(false)
      val newBody = newForStmt.body.asInstanceOf[StatementBlock]

      stmtList.foreach(s => {
        if (blk.statements.contains(s)) {
          newBody.appendStatement(s)
        } else {
          //Find parent node of the assignment statement
          var parent: Statement = s.getParent
          while (parent != null) {
            if (blk.statements.contains(parent)) {
              newBody.appendStatement(parent)
              parent = null
            } else {
              parent = parent.getParent
            }
          }
        }
      })

      parentBlk.insertBefore(forStmt, newForStmt)
    })
    // Remove 'for' block
    parentBlk.removeStatement(forStmt)
  }
  
  def processOriginalForStatement(forStmt: ForStatement, sorted: List[List[AssignmentStatement]]){
    LoopHelper.hasValidForHeader(forStmt) map {
      case loopInfo => {
        val blk = forStmt.body.asInstanceOf[StatementBlock]
        
        blk.statements.foreach(s => s.props[DomainsProperty].prependDomain(loopInfo))        
        val parentBlk = forStmt.getParent.asInstanceOf[StatementBlock]
        for (s <- blk.statements) yield {
          parentBlk.insertBefore(forStmt, s)
        }
        // Remove 'for' block
        parentBlk.removeStatement(forStmt)
      }
    }
  }
}

object FadaLoopSplittingPass {
  def doPass(tu: TranslationUnit): Unit = {
    StatementProcessor.process(tu, new FadaLoopSplittingPass())
  }
  
  def doPass(function: FunctionDef): Unit = {
//    println("********************************************************************************")
//    println("Original Function: ")
//    println(function.pretty())
    
    val newFunc = function.cloneFunc
    UniqueLoopIndexPass.doPass(newFunc)
    
    val cfg = SFGBuilder.build(newFunc)
    SSAPass.insertPhi(cfg)    
//    println("********************************************************************************")
//    println("After SSA:")
//    println(newFunc.pretty())
//    println("********************************************************************************")
    
    
    StatementProcessor.process(newFunc, new FadaLoopSplittingPass())
    
//    println("********************************************************************************")
//    println("All Splitted:")
//    println(newFunc.pretty())
  }
}
