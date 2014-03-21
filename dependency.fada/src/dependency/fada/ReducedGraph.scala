package dependency.fada

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scalax.collection.mutable.Graph

import  model._
import  model.statement._
import  core.util.DotDescriptor

case class ReducedGraph(stmts: List[AssignmentStatement], graph: Graph[Set[AssignmentStatement], SCCDistVecEdge]){
  // Prints list of original statements followed by SCC graph
  override def toString() = {
    val buf = new StringBuilder
    def println(s: String) = buf ++= s; buf ++= "\n"
    
    def getHash(stmt: AssignmentStatement) = {
      //stmt.toString.substring(stmt.toString.indexOf("@"))
      stmt[FadaId].id.toString
    }
    println("[Reduced Graph]")
    println("Statements:")
    for (s <- stmts) {
      println(getHash(s) + ": " + s.pretty())
    }
    println("Nodes:" + graph.nodes.size)
    for (n <- graph.nodes) {
      val str = n.map(s => getHash(s)).mkString("(", ", ", ")")
      println(str)
    }
    // Edges should be zero
    println("Edges:" + graph.edges.size)
    for (e <- graph.edges) {
      val set1 = e._1
      val set2 = e._2
      val str1 = set1.map(s => getHash(s)).mkString("(", ", ", ")")
      val str2 = set2.map(s => getHash(s)).mkString("(", ", ", ")")
      val dstr = e.distVec.vectors.map(v => v.pretty()).mkString(", ")
      println(str1 + " -> " + str2 + " : " + dstr)
    }
    println("[EndGraph]")
    
    buf.toString
  }
  
  // Topological sort of SCCs in graph
  // Note: graph is destroyed after call
  def topoSort() = {
    //val order = stmts.zipWithIndex.toMap
    val order = stmts.map(s => (s -> s[FadaId].id)).toMap
    val filterNodes = graph.nodes.filter(n => n.inDegree == 0).map(n => n.value)
    val noIncomingNodes = ListBuffer.empty[Set[AssignmentStatement]] ++ filterNodes.toList.sortWith((n1, n2) => order(n1.head) < order(n2.head))
    val sortedSCCs = ListBuffer.empty[Set[AssignmentStatement]]
    
    while (noIncomingNodes.size > 0) {
      val node = noIncomingNodes.head
      noIncomingNodes -= node
      sortedSCCs += node
      val dstEdges = graph.edges.filter(e => e.from == node)
      for (e <- dstEdges) {
        graph -= e
        val m = graph.get(e.target)
        if (m.diPredecessors.isEmpty)
          noIncomingNodes += m.value
      }
    }
    
    if (!graph.edges.isEmpty) throw new UnsupportedOperationException("Graph has cycles")
    else {
      val sortedStmts = ListBuffer.empty[List[AssignmentStatement]]
      for (scc <- sortedSCCs) {
        sortedStmts += scc.toList.sortWith((n1, n2) => order(n1) < order(n2))
      }
      sortedStmts.toList
    }
  }
  
  def toDot(): String = {
    val dotNodes = scala.collection.mutable.Map[Int, DotDescriptor]()
    val dotEdges = scala.collection.mutable.ListBuffer[DotDescriptor]()

    val sets = graph.nodes.toList
    sets.zipWithIndex.foreach {
      case (nodeT, id) => {
        val set = nodeT.value
        
        val nodeDescription = id.toString
        val d = new DotDescriptor(nodeDescription)
        d.attributes += "shape" -> "box"
        d.attributes += "label" -> String.format("\"%s\"", 
            DotDescriptor.strEscape(
                set.map(stmt => stmt[FadaId].id + ": " + stmt.pretty()).mkString("\n")
            ))

        dotNodes += id -> d

        graph.get(set).outgoing.foreach(edge => {
          val description = sets.indexOf(edge.from) + " -> " + sets.indexOf(edge.to)
          dotEdges += new DotDescriptor(description)
        })
      }
    }
    
    val s = new StringBuilder();
    s.append("digraph \"Reduced Dependency Graph\" {\ngraph [ordering=out];\n");
    for (d <- dotNodes.values) {
      s.append(d)
      s.append("\n")
    }
    for (d <- dotEdges) {
      s.append(d)
      s.append("\n")
    }
    s.append("}\n")
    
    s.toString
  }
}