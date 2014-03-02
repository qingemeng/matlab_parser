package matlabParser

import scala.util.parsing.combinator._

import model._
import model.expression._
import model.statement._
import refactoring.matlab._
import refactoring.matlab.processing.CodeGenerator._

/**
 * Created with IntelliJ IDEA.
 * User: gemengqin
 * Date: 12/30/13
 * Time: 10:12 PM
 * To change this template use File | Settings | File Templates.
 */
object Main extends Parsers{
  def main(args : Array[String]){
    val path = "/Users/gemengqin/Dropbox/ntu/FYP/matlab_to/src/m_files"
    val testPath = "/Users/gemengqin/Dropbox/ntu/FYP/matlab_to/src/m_files/test"

    val plusTest = "/arith/plus.m"
    val testAssign  ="/test_assignment.m"
    val testCell = "/test_cell.m"
    val testCtrl = "/test_cntrl.m"
    val testComments = "/test_comments.m"
    val test_if_only = "/test_if.m"
    val test_if_else_only = "/test_if_else.m"
    val test_if = "/test_else_if.m"
    val testElse_If = "/test_elseif2.m"
    val testElseIf_Simple = "/test_elseif_simple.m"
    val testFor ="/test_for.m"
    val testExpr ="/test_expr.m"
    val testMatrix ="/test_matrix.m"
    val testNestedFuncs ="/test_nested_funcs.m"
    val testSingleLineStmt = "/test_singleLineStmt.m"
    val testSwitch="/test_switch.m"
    val testWhile="/test_while.m"
    val testScalar="/test_scalar.m"
    val testVR ="/test_vector_row.m"
    val testVC ="/test_vector_col.m"
    val testNAryOp = "/test_NAryOp.m"
    val testArrRef = "/test_simpleMatrixAccessExpr.m"
    val testArrOp_plus = "/test_arrOP_plus_minus.m"
    val testArrOp_arrTimes = "/test_arrayOp_arrTimes.m"
    val testArrOp_matTimes = "/test_matOp_matTimes.m"
    val test_ = "/test_.m"


    //stencil code

    val test_stencil_demo = "/test_paralleldemo_gpu_stencil.m"
    val t_matrixCreation  = "/t_matrixCreation.m"





    val filename = testPath + testCtrl


    val content = scala.io.Source.fromFile(filename).mkString

    MatlabParser.parseSource(content) match {
      case Left((declMap, stmt)) => {
        //println(declMap.mkString("\n"));

        val pretty = stmt.pretty()
        println(pretty)
//
        val tree_pretty = stmt.treePretty()
        println(tree_pretty)

        val tree_type_pretty  = stmt.typePretty();
        println(tree_type_pretty)

        println(generate(stmt))

//        MatlabParser.parseStatement(pretty) match {
//          case Left((declMap2, stmt2)) => println(stmt2.pretty())
//            println(stmt2.treePretty())
//
//          case Right(err2) => println(err2)
//        }

      }
      //case Left((declMap, func))=>
      case Right(err) => println(err)
    }

  }


}
