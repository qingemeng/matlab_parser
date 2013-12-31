package   core

import   model._

abstract class DeclUseGroup {
  def getDeclarationType: BasicType

  //----------------------- get more information for specific types
  def isArray: Boolean = {
    getArrayInfo != null
  }
  
  def getArrayInfo: ArrayInfo
  def updateArrayInfo(arrayInfo: ArrayInfo)
}

