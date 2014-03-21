package dependency.fada

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap


import  model._
import  model.statement._
import  core.util.DotDescriptor
import scalax.collection.mutable.Graph

case class DependencyGraph(stmts: List[AssignmentStatement], graph: Graph[AssignmentStatement, DistVecEdge]) {
  // Prints list of original statements followed by graph
  override def toString() = {
    val buf = new StringBuilder
    def println(s: String) = buf ++= s; buf ++= "\n"
    
    def getHash(stmt: AssignmentStatement) = {
      //stmt.toString.substring(stmt.toString.indexOf("@"))
      stmt[FadaId].id.toString
    }
    println("[Raw Graph]")
    println("Statements:")
    for (s <- stmts) {
      println(getHash(s) + ": " + s.pretty())
    }
    println("Nodes:" + graph.nodes.size)
    for (n <- graph.nodes) {
      println(getHash(n))
    }
    println("Edges:" + graph.edges.size)
    for (e <- graph.edges) {
      val str1 = getHash(e._1)
      val str2 = getHash(e._2)
      val dstr = e.distVec.vectors.map(v => v.pretty()).mkString(", ")
      println(str1 + " -> " + str2 + " : " + dstr)
    }
    println("[EndGraph]")
    
    buf.toString
  }
  
  // Returns graph with condensed SCCs using Tarjan's algorithm
  def reduceSCC() = {
    var index = 0
    val nodeIndex = HashMap.empty[graph.NodeT, Int] ++ graph.nodes.map(n => (n, -1))
    val nodeLowLink = HashMap.empty[graph.NodeT, Int] ++ graph.nodes.map(n => (n, -1))
    var stack = ListBuffer.empty[graph.NodeT]
    val SCCs = ListBuffer.empty[Set[AssignmentStatement]]

    def identifySCC(v: graph.NodeT): Unit = {
      nodeIndex(v) = index
      nodeLowLink(v) = index
      index += 1
      stack.prepend(v)
      
      for (w <- v.diSuccessors) {
        if (nodeIndex(w) == -1) {
          identifySCC(w)
          nodeLowLink(v) = scala.math.min(nodeLowLink(v), nodeLowLink(w))
        } else if (stack.contains(w)) {
          nodeLowLink(v) = scala.math.min(nodeLowLink(v), nodeIndex(w))
        }
      }
      
      if (nodeLowLink(v) == nodeIndex(v)) {
        val idx = stack.indexOf(v)
        val scc = stack.take(idx+1)
        stack = stack.drop(idx+1)
        SCCs.append(scc.map(n => n.value).toSet)
      }
    }
    
    // identify SCCs
    for (n <- graph.nodes) {
      if (nodeIndex(n) == -1)
        identifySCC(n)
    }

    // after identifying SCCs, rebuild graph and combine edges
    def findSCC(node: AssignmentStatement) = { SCCs.find(scc => scc.contains(node)).get }
    val newEdges = ListBuffer.empty[SCCDistVecEdge[Set[AssignmentStatement]]]
    for (fromSCC <- SCCs) {
      // find edges originating from scc and ending outside scc
      val srcEdges = graph.edges.filter(e => fromSCC.contains(e.from))
      val dstEdges = srcEdges.filter(e => !fromSCC.contains(e.to))
      // group edges by destination scc containing the to-node
      val edgesGroup = dstEdges.groupBy(e => findSCC(graph get e.to))
      for ((toSCC, edges) <- edgesGroup) {
        // edges are assumed to be all positive, just choose first one
        newEdges += SCCDistVecEdge(fromSCC, toSCC, edges.head.distVec)
      }
    }

    val newGraph = Graph.empty[Set[AssignmentStatement], SCCDistVecEdge]
    SCCs.foreach { scc => newGraph.add(scc) }
    newEdges.foreach { e => newGraph.add(e) }

    ReducedGraph(stmts, newGraph)
  }
  
  def toDot(): String = {
    val dotNodes = scala.collection.mutable.Map[Int, DotDescriptor]()
    val dotEdges = scala.collection.mutable.ListBuffer[DotDescriptor]()
    
    for (n <- graph.nodes) {
      val id = n[FadaId].id
      val nodeDescription = id.toString
      val d = new DotDescriptor(nodeDescription)
      d.attributes += "shape" -> "box"
      d.attributes += "label" -> String.format("\"%s\"", DotDescriptor.strEscape(id + ": " + n.pretty()))

      dotNodes += id -> d
      
      graph.get(n).outgoing.foreach(edge => {

        val description = edge.source[FadaId].id + " -> " + edge.target[FadaId].id
        dotEdges += new DotDescriptor(description)        
      })
    }
    
    val s = new StringBuilder();
    s.append("digraph \"Dependency Graph\" {\ngraph [ordering=out];\n");
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