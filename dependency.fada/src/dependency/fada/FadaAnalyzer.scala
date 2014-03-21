package dependency.fada

import scala.collection.JavaConversions._
import scalax.collection.mutable.Graph
import com.hpctoday.fada._
import  model._
import  model.expression._
import  model.statement._
import  core.processing._
import StatementType._
import scalax.collection.mutable.Graph

// Conversion to SSA will remove the anti-dependencies

// * Must be preceded by SSA conversion
object FadaAnalyzer {
  private def buildGraph(dslStmt: DSLStatement, generateArray: Boolean) = {
    //println("********************************************************************************")
    //println("Original For")
    //println(stmt.pretty())
    
    val (program, dslStmtList) = FadaStmtBuilder.generateProgram(dslStmt, generateArray)
    //println(program.Regenerate_C_Code("\n"))
    
  	val fProg = new Program()
  	fProg.SetSyntaxTree(program)
  	fProg.ComputeSourcesForAllReadVariables()
  		
  	val references = fProg.GetNormalizedStmts()        
    
    //Collect ids of the child assignment statements
    val childIds = {
      val ids = scala.collection.mutable.ListBuffer[Int]()
      StatementProcessor.process(dslStmt, new StatementVisitor(){
        override def visit(stmt: AssignmentStatement): Int = {
          if(dslStmtList.contains(stmt)) ids += dslStmtList.indexOf(stmt)
          StatementVisitor.Continue
        }
      })
      ids.sorted
    }

    //val childIds = dslStmtList.filterNot(_.isInstanceOf[AssignmentStatement]).map(_[FadaId].id).sorted
    
    //println("childIds: " + childIds)
    val remainingNodes = scala.collection.mutable.Set(childIds:_*)

    //println(loopInfo.inductionVar.name + ": " + stmt.body.asInstanceOf[StatementBlock].statements.size + " => " + childIds)

    val graph = Graph.empty[AssignmentStatement, DistVecEdge]
    childIds.foreach(childId => {
      val s2 = dslStmtList(childId).asInstanceOf[AssignmentStatement]

      val ref = references(childId)

      def buildDependency(quast: Quast) {
        if (!quast.IsLeaf()) {
          buildDependency(quast.GetThenPart())
          buildDependency(quast.GetElsePart())
        } else if (!quast.IsEmpty()) {
          val readId = quast.GetAssignment()

          dslStmtList(readId) match {
            case s1: AssignmentStatement => {
              //If statement within the loop
              if (childIds.contains(readId)) {
                val indexVector = quast.GetVertex().GetIndex()

                //dv is the read index vector
                val dv = indexVector.map(DslExprBuilder.generateExpression(_)).toList

                graph.add(DistVecEdge(s1, s2, DistanceVectors(dv)))

                remainingNodes -= childId
                remainingNodes -= readId
              }
            }

            case _ => System.err.println("Dependency on non-assignment statement: " + dslStmtList(readId).pretty())
          }
        }
      }
      ref.GetRV().map(read => {
        buildDependency(read.GetDefinition())
      })

    })
    
    // finally, add nodes which are not in 'edges' list
    remainingNodes.foreach(n => {
      graph.add(
       	dslStmtList(n).asInstanceOf[AssignmentStatement]
      )
    })

    val assignStmts = childIds.map(id => dslStmtList(id).asInstanceOf[AssignmentStatement]).toList
    
    val depGraph = DependencyGraph(assignStmts, graph)
    depGraph
  }
  
  def buildArrayDependencyGraph(stmt: DSLStatement) = buildGraph(stmt, true)
  def buildDSLDependencyGraph(stmt: DSLStatement) = buildGraph(stmt, false)
  
  def buildSCCandSort(stmt: ForStatement) = {
    val depGraph = buildArrayDependencyGraph(stmt)
    val reducedGraph = depGraph.reduceSCC
    val sorted = reducedGraph.topoSort
    
    //printSortedList(sorted)
    
    sorted
  }
  
  def printSortedList(list: List[List[AssignmentStatement]]){
    println("[Sorted List]")
    list.zipWithIndex.foreach {
      case (l, i) => {
        println("Group: " + i)
        l.foreach(s => {
          println(s[FadaId].id + ": " + s.pretty())
        })
        println()
      }
    }
    println("[End List]")
  }
}
