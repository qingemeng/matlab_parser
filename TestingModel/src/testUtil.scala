import collection.mutable.Stack
import matlabParser.MatlabParser
import scala.util.parsing.combinator._

import org.scalatest._
import MatlabParser._

object TestUtil{
  val verbose_tree = false
  val verbose_treePretty = false

  // util functions
  def parsing_script(text:String) {
    val result = MatlabParser.parseAll(source, text)
    if(result.successful==true && verbose_tree==true){
      declMap.clear()



    }


    assert(result.successful == true)
  }

  def parsing_id(id:String){
    val result = MatlabParser.parseAll(identifierName,id)
    assert(result.successful==true)
  }

}