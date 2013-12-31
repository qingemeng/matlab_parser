package model.property

trait HasProperties {
	val props = PropertyMap()
	
	def apply[A : Manifest]: A = props[A]
	
	def copyProperties(src: HasProperties){
	  props.copy(src.props)
	}
}