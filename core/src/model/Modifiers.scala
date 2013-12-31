package   model

sealed trait Modifier
case class Constant() extends Modifier
case class Restricted() extends Modifier
case class Unsigned() extends Modifier