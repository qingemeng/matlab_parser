package dependency.fada

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._
import scalax.collection.mutable.Graph

import  core._
import  model._
import  model.statement._
import  model.expression._
import  core.util.DotDescriptor
import  model.NumericValue.IntBox


case class DistanceVectors(val vectors: List[Expr])

// Edge for statement nodes
object DistVecEdge {
  def apply(from: AssignmentStatement, to: AssignmentStatement, distVec: DistanceVectors) = new DistVecEdge[AssignmentStatement](NodeProduct(from, to), distVec)
  def unapply(e: DistVecEdge[AssignmentStatement]) = Some(e)
}
class DistVecEdge[AssignmentStatement](nodes: Product, val distVec: DistanceVectors) 
  extends DiEdge[AssignmentStatement](nodes)
  with EdgeCopy[DistVecEdge]
  with EdgeIn[AssignmentStatement, DistVecEdge] {
  
  override def copy[S](newNodes: Product) =
    new DistVecEdge[S](newNodes, distVec)
}

final class DistVecEdgeAssoc[A <: AssignmentStatement](val e: DiEdge[A]) {
  @inline def ## (distVec: DistanceVectors) = new DistVecEdge[A](e.nodes, distVec) with EdgeIn[A, DistVecEdge]
  
  implicit def edge2DistVecEdgeAssoc[A <: AssignmentStatement](e: DiEdge[A]) = new DistVecEdgeAssoc[A](e)
}

// Edge for set of statement nodes
class SCCDistVecEdge[Set](nodes: Product, val distVec: DistanceVectors) 
  extends DiEdge[Set](nodes)
  with EdgeCopy[SCCDistVecEdge]
  with EdgeIn[Set, SCCDistVecEdge] {
  
  override def copy[S](newNodes: Product) =
    new SCCDistVecEdge[S](newNodes, distVec)
}
object SCCDistVecEdge {
  def apply(from: Set[AssignmentStatement], to: Set[AssignmentStatement], distVec: DistanceVectors) = new SCCDistVecEdge[Set[AssignmentStatement]](NodeProduct(from, to), distVec)
  def unapply(e: SCCDistVecEdge[Set[AssignmentStatement]]) = Some(e)
}

final class SCCDistVecEdgeAssoc[A <: Set[AssignmentStatement]](val e: DiEdge[A]) {
  @inline def ## (distVec: DistanceVectors) = new SCCDistVecEdge[A](e.nodes, distVec) with EdgeIn[A, SCCDistVecEdge]
  
  implicit def edge2DistVecEdgeAssoc[A <: Set[AssignmentStatement]](e: DiEdge[A]) = new SCCDistVecEdgeAssoc[A](e)
}


//object DependencyGraph2 {
////  // Build dependency graph with SCC reduction
////  def buildSCC(stmts: List[AssignmentStatement], inducVar: IdName) = {
////    var g = build(stmts, inducVar)
////    //DependencyGraph.printRawGraph(stmts, g)
////    g = modifyEdges(g)
////    //DependencyGraph.printRawGraph(stmts, g)
////    val rg = reduceSCC(g)
////    //DependencyGraph.printReducedGraph(stmts, rg)
////    rg
////  }
//  
//
//  
//  // Builds a dependency graph for the given list of statements and the induction variable
//  // Each edge can have many distance vectors (eg. multiple inducVar in indices)
//  // List of statements are assumed to be in lexicographic order
//  def build(stmts: List[AssignmentStatement], inducVar: IdName) = {
//    val edges = ListBuffer.empty[(AssignmentStatement, AssignmentStatement, List[Expr])]
//    //val lexorder = stmts.zipWithIndex.toMap
//    val lexorder = stmts.map(s => (s -> s[FadaId].id)).toMap
//    for (s1 <- stmts) {
//      for (s2 <- stmts) {
//        val write = s1.writeRef
//        val reads = s2.readRefs
//        var dists = reads.map(rd => calcDistanceVector(inducVar, write, rd))
//
//        // remove lexically backward arrows whose distance is zero
//        // and where s2 writes to same array as s1
//        if (lexorder(s2) < lexorder(s1)) {
//          if (s2.writeRef == s1.writeRef) {
//            dists = dists.filter(d => d match {
//              case None => false
//              case Some(xprs) => 
//                if ((xprs.size == 1) && (xprs(0).isInstanceOf[ConstLiteralExpr])) {
//                  val c = xprs(0).asInstanceOf[ConstLiteralExpr]
//                  !(c.isNumeric && (c.numeric == 0)) 
//                } else false   
//            })
//          }
//        }
//        
//        // combine the distance vectors
//        val comb = dists.reverse.foldLeft(List.empty[Expr])((acc, d) => d match {
//          case None    => acc
//          case Some(l) => l:::acc
//        })
//        if (comb.size > 0) {
//          edges += ((s1, s2, comb))
//        }
//      }
//    }
//
//    val graph = Graph.empty[AssignmentStatement, DistVecEdge]
//    var nodes = stmts.toSet
//    
//    for (e <- edges) {
//      val (s1, s2, dv) = e
//      graph.add(DistVecEdge(s1, s2, DistanceVectors(dv)))
//      nodes -= s1
//      nodes -= s2
//    }
//    // finally, add nodes which are not in 'edges' list
//    for (n <- nodes) {
//      graph.add(n)
//    }
//
//    graph
//  }
//  
//  // Returns a list of distance vectors between two expressions, e1 write e2 read
//  private def calcDistanceVector(inducVar: IdName, e1: Expr, e2: Expr): Option[List[Expr]] = {
//    e1 match {
//      case e: IdExpr       => if (e1 == e2) Some(List(ConstLiteralExpr(0))) else None
//      case e: FieldRefExpr => if (e1 == e2) Some(List(ConstLiteralExpr(0))) else None
//      case e: ArrayRefExpr =>
//        if (!e2.isInstanceOf[ArrayRefExpr]) return None
//        val a1 = e
//        val a2 = e2.asInstanceOf[ArrayRefExpr]
////        if (a1 == a2) {
////          return Some(List(ConstLiteralExpr(0))) 
////        }
//        if ((a1.owner == a2.owner) && (a1.rank == a2.rank)) {
//          val zipIdx = a1.indices.zip(a2.indices) 
//          val diffIdx = zipIdx.filter(i => containsInducVar(inducVar, i._1) || containsInducVar(inducVar, i._2))
//          var distSet = diffIdx.map(i => Algebra.simplify(i._1 - i._2))
//
//          //if (distSet.size == 0) return None
//          // if size == 0, use a conservative estimate based on a1.owner == a2.owner
//          if (distSet.size == 0) return Some(List(ConstLiteralExpr(0)))
//          else return Some(distSet)
//        }
//        return None
//      case _ => None
//    }
//  }
//  
//  private def containsInducVar(inducVar: IdName, expr: Expr): Boolean = {
//    var found = false
//    ExpressionProcessor.process(expr, new ExpressionVisitor() {
//      override def visit(expr: IdExpr): Int = {
//        if (expr.idName == inducVar) found = true
//        return ExpressionVisitor.Abort
//      }
//    })
//    return found
//  }
//  
//  // For edges with negative distance, flip the dependency arrows
//  // For edges where distance is non literal, cannot be evaluated, 
//  //   add arrow so that it becomes an SCC, thus no code motion allowed
//  def modifyEdges(graph: Graph[AssignmentStatement, DistVecEdge]) = {
//    // negative edges
//    val negEdges = graph.edges.filter { e =>
//      if (e.distVec.vectors.size == 1 && e.distVec.vectors(0).isInstanceOf[ConstLiteralExpr]) {
//        val c = e.distVec.vectors(0).asInstanceOf[ConstLiteralExpr]
//        c.numeric < 0
//      } else false
//    }
//    // complex edges
//    val comEdges = graph.edges.filter { e =>
//      if (e.distVec.vectors.size > 1 || !e.distVec.vectors(0).isInstanceOf[ConstLiteralExpr]) true
//      else false
//    }
//
//    negEdges.foreach(e => graph -= e)
//    negEdges.foreach { e => 
//      val from = e._2
//      val to = e._1
//      val origdist = e.distVec.vectors(0)
//      val expr = Algebra.multiplyConsts(List(ConstLiteralExpr(-1), origdist))
//      val dv = DistanceVectors(List(expr))
//      graph.add(DistVecEdge(from, to, dv))
//    }
//    
//    comEdges.foreach { e =>
//      val from = e._2
//      val to = e._1
//      val dv = e.distVec
//      graph.add(DistVecEdge(from, to, dv))
//    }
//    
//    graph
//  }
//}