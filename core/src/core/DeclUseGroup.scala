package   core

import   model._
import org.eclipse.cdt.core.dom.ast.IASTName

abstract class DeclUseGroup {
  var declNames:List[IASTName]
  def getDeclarationType: BasicType

  //----------------------- get more information for specific types
  def isArray: Boolean = {
    getArrayInfo != null
  }
  
  def getArrayInfo: ArrayInfo
  def updateArrayInfo(arrayInfo: ArrayInfo)
//  def getDecl
}

