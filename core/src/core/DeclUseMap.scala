package   core

import   model._

// Declaration-Use(rw) Map
abstract class DeclUseMap {
  def getDeclUse(idName: IdName): DeclUseGroup
  def declExist(idName: IdName): Boolean
  def addDecl(idName: IdName, declUseGroup: DeclUseGroup)
}