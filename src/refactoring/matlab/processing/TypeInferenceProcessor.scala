package refactoring.matlab.processing

import model.statement._
import model.BasicType
import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: gemengqin
 * Date: 2/20/14
 * Time: 4:41 PM
 * To change this template use File | Settings | File Templates.
 */
object TypeInferenceProcessor {
  def typeInference(ast: StatementBlock) = {
    new TypeInferenceProcessor().typeInference(ast)
  }
  def typeInference(stmt:Statement) ={
    new TypeInferenceProcessor().typeInference(stmt)
  }
}

class TypeInferenceProcessor   {
  var typeInfo: Map[String, BasicType] = Map.empty

  def typeInference(ast: StatementBlock)= {
    // derive type information
    typeInfo = SimpleTypeInferencer.infer(ast)
    typeInfo
  }
  def typeInference(stmt: Statement)= {
    // derive type information
    typeInfo = SimpleTypeInferencer.infer(stmt)
    typeInfo
  }
}
