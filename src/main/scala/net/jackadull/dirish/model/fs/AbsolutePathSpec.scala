package net.jackadull.dirish.model.fs

// TODO
sealed trait AbsolutePathSpec {
  def /(element:String):AbsolutePathSpec = ??? // TODO
  def /(rel:RelativePathSpec):AbsolutePathSpec = ??? // TODO
  def lastElement:String = ??? // TODO
  def modLastElement(f:String⇒String):AbsolutePathSpec = ??? // TODO
}
