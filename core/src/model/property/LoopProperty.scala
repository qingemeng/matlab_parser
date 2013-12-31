package model.property

// For indicating whether loop can be abstracted
// Used for debugging purposes
class LoopProperty() extends Property{
  private var _canAbstractLoop = false
  def setCanAbstractLoop(canAbstract: Boolean) { _canAbstractLoop = canAbstract }
  def getCanAbstractLoop = _canAbstractLoop
  
  override def cloneProperty(): Property = {
    val newProp = new LoopProperty()
    newProp._canAbstractLoop = this._canAbstractLoop
    newProp
  }
}