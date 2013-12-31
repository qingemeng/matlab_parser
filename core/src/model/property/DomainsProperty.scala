package model.property

import   model._

// For storing LoopInfo(s) within statements 
class DomainsProperty extends Property{

  // Domains ordered left to right = outer to inner
  private var _domains: List[LoopInfo] = List.empty

  // Flag to indicate if model.statement can be lifted
  def hasDomains: Boolean = !_domains.isEmpty
  def getDomains = _domains
  def prependDomain(domain: LoopInfo): this.type = { _domains = domain::_domains; this } 
  def clearDomains { _domains = List.empty }
  def copyDomains(domains: List[LoopInfo]) {_domains = domains}
  
  override def cloneProperty(): Property = {
    val prop = new DomainsProperty()
    prop._domains = this._domains
    prop
  }
}