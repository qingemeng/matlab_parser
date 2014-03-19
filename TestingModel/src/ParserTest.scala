import org.scalatest._
import TestUtil._

class ParserTest extends FlatSpec with Matchers {
  //setting up patth

  val testAssign  ="/test_assignment.m"
  val testPath = "/Users/gemengqin/Dropbox/ntu/FYP/matlab_to/src/m_files/test"


  "Id parsing" should "parse letters" in {

    val letters = "a"
    val letters2 = "abc"
    parsing_id(letters)
    parsing_id(letters2)
  }

  "Id parsing" should "parse letters with numbers" in {
       val t1 = "a2"
    val t2 = "abcs"
    parsing_id(t1)
    parsing_id(t2)
  }

  "single line statement parsing" should "parse unary expression as a single line expression" in {

    implicit val parserToTest = Boolean
    val testSingleLineStmt  ="/test_singleLineStmt.m"
    val filename = testPath + testSingleLineStmt
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }

  "assignment statement parsing" should "parse simple assignment statement" in {

    implicit val parserToTest = Boolean
    val testAssign  ="/test_assignment.m"
    val filename = testPath + testAssign
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }

  "for statement parsing" should "parse for statement" in {

    val testFor  ="/test_for.m"
    val filename = testPath + testFor
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }

  "cell statement parsing" should "parse cell statement" in {
    //just declare the parser to test once and mark it implicit
    //that way our test functions will use it automagically
    val testCell  ="/test_cell.m"
    val filename = testPath + testCell
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }

  "continue(control) statement parsing" should "parse continue statement" in {

    val testCntrl  ="/test_cntrl.m"
    val filename = testPath + testCntrl
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }

  "comments parsing" should "skip all comments" in {

    val testComment  ="/test_comments.m"
    val filename = testPath + testComment
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }

  "simple elseif parsing" should "parse elseif in if statement" in {

    val testElseifS  ="/test_elseif_simple.m"
    val filename = testPath + testElseifS
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }

  "if statement parsing" should "parse (only)if statement" in {

    val testIf  ="/test_if.m"
    val filename = testPath + testIf
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }
  "if statement with else parsing" should "parse if statement with else block" in {

    val testIfElse  ="/test_if_else.m"
    val filename = testPath + testIfElse
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }

  "elseif statement parsing" should "parse elseif in if statement" in {

    val testElseif  ="/test_else_if.m"
    val filename = testPath + testElseif
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }


  "matrix statement parsing"should "parse simple scalar" in {

    val testScalar  ="/test_scalar.m"
    val filename = testPath + testScalar
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }

  "matrix statement parsing"should "parse simple row vector" in {

    val testVR  ="/test_vector_row.m"
    val filename = testPath + testVR
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }
  "matrix statement parsing"should "parse simple column vector" in {

    val testVC  ="/test_vector_col.m"
    val filename = testPath + testVC
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }

  "NAryOp parsing"should "parse + - /" in {

    val test  ="/test_NAryOp.m"
    val filename = testPath + test
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }
  "Matrix access parsing"should "parse simple ArrRefExpr" in {

    val test  ="/test_simpleMatrixAccessExpr.m"
    val filename = testPath + test
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }

  // operators

  "Array Op parsing"should "parse simple Array op as + -" in {

    val test  ="/test_arrOP_plus_minus.m"
    val filename = testPath + test
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }

  "Array Op parsing"should "parse Array times" in {

    val test  ="/test_arrayOp_arrTimes.m"
    val filename = testPath + test
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }

  "Array Op parsing"should "parse Matrix times" in {

    val test  ="/test_matOp_matTimes.m"
    val filename = testPath + test
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }
  "Array unary Op parsing"should "parse +A -A" in {

    val test  ="/test_matOp_matTimes.m"
    val filename = testPath + test
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }

  //unary ops


  "simple stencil parsing"should "parse simple stencil code" in {

    val test  ="/test_paralleldemo_gpu_stencil.m"
    val filename = testPath + test
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }













//  "The MatlabParser" should "parse a big test statement" in {
//    //just declare the parser to test once and mark it implicit
//    //that way our test functions will use it automagically
//    val test_test  ="/big_test.m"
//    val filename = testPath + test_test
//    val content = scala.io.Source.fromFile(filename).mkString
//    parsing_script(content)
//  }


//  "A Stack" should "pop values in last-in-first-out order" in {
//    val stack = new Stack[Int]
//    stack.push(1)
//    stack.push(2)
//    stack.pop() should be (2)
//    stack.pop() should be (1)
//  }
//
//  it should "throw NoSuchElementException if an empty stack is popped" in {
//    val emptyStack = new Stack[Int]
//    a [NoSuchElementException] should be thrownBy {
//      emptyStack.pop()
//    }
//  }
}