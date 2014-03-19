package   core

import   model._
import org.eclipse.cdt.core.dom.ast.IASTName

class BaseDeclUseGroup(_type: BasicType,var _arrayInfo: ArrayInfo) extends DeclUseGroup {
  def this(_type: BasicType) = this(_type, null)
  
  override def getDeclarationType = _type
  
  override def getArrayInfo: ArrayInfo = _arrayInfo
  override def updateArrayInfo(arrayInfo: ArrayInfo){
    _arrayInfo = arrayInfo
  }

  var declNames: List[IASTName] = _
}