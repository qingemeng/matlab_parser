package   core

trait NotImplemented {
  def ??? : Nothing = throw new NotImplementedError
  def ???(s: String): Nothing = throw new NotImplementedError(s)
}