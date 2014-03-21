package dependency.fada

import  model.property.Property

class FadaId extends Property {
  var id:Int = 0
  
  override def cloneProperty(): Property = {
    val newProp = new FadaId()
    newProp.id = id
    newProp
  }
}