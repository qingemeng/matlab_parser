package   core

import   model._

class BaseDeclUseMap extends DeclUseMap {
  private val _cache = scala.collection.mutable.HashMap.empty[String, DeclUseGroup]

  override def getDeclUse(idName: IdName): DeclUseGroup = _cache(idName.id)
  override def declExist(idName: IdName): Boolean = _cache.contains(idName.id)
  
  override def addDecl(idName: IdName, declUseGroup: DeclUseGroup){
    _cache(idName.id) = declUseGroup
  }
}