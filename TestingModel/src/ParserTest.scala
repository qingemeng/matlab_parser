import collection.mutable.Stack
import matlabParser.MatlabParser
import scala.util.parsing.combinator._
import org.scalatest._
import TestUtil._

class ParserTest extends FlatSpec with Matchers {
  //setting up patth

  val testAssign  ="/test_assignment.m"
  val testPath = "/Users/gemengqin/Dropbox/ntu/FYP/matlab_to/src/m_files/test"


  "Id parsing" should "parse letters" in {
    //just declare the parser to test once and mark it implicit
    //that way our test functions will use it automagically
    val letters = "a"
    val letters2 = "abc"
    parsing_id(letters)
    parsing_id(letters2)
  }

  "Id parsing" should "parse letters with numbers" in {
    //just declare the parser to test once and mark it implicit
    //that way our test functions will use it automagically
    val t1 = "a2"
    val t2 = "abcs"
    parsing_id(t1)
    parsing_id(t2)
  }

  "assignment statement parsing" should "parse simple assignment statement" in {
    //just declare the parser to test once and mark it implicit
    //that way our test functions will use it automagically
    implicit val parserToTest = Boolean
    val testAssign  ="/test_assignment.m"
    val filename = testPath + testAssign
    val content = scala.io.Source.fromFile(filename).mkString
    parsing_script(content)
  }

  "for statement parsing" should "parse for statement" in {
    //just declare the parser to test once and mark it implicit
    //that way our test functions will use it automagically
    val testFor  ="/test_for.m"
    val filename = testPath + testFor
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