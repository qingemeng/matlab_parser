package matlabParser

import scala.util.parsing.combinator._

import model._
import model.expression._
import model.statement._

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
    val testCell = "/test_cell.m"
    val testMatrix  =""


    val filename = testPath + testCell
    val content = scala.io.Source.fromFile(filename).mkString

    MatlabParser.parseSource(content) match {
      case Left((declMap, stmt)) => {
        //println(declMap.mkString("\n"));

        val pretty = stmt.pretty()
        val tree_pretty = stmt.treePretty()

        MatlabParser.parseStatement(pretty) match {
          case Left((declMap2, stmt2)) => println(stmt2.pretty())
            println(stmt2.treePretty())

          case Right(err2) => println(err2)
        }

      }
      case Right(err) => println(err)
    }

  }


}
