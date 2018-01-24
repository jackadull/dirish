package net.jackadull.dirish.path

import scala.language.postfixOps

sealed trait PathSpec {
  def /(that:RelativePathSpec):PathSpec
  def /(that:String):PathSpec
  private[path] def elements:Stream[PathSpec]
}

sealed trait AbsolutePathSpec extends PathSpec {
  override def /(that:RelativePathSpec):AbsolutePathSpec = that match {
    case element:PathElementSpec ⇒ CompositeAbsolutePathSpec(this, element)
    case CompositeRelativePathSpec(childsHead, childsTail) ⇒ CompositeAbsolutePathSpec(this/childsHead, childsTail)
  }
  override def /(that:String):AbsolutePathSpec = this/PathElementSpec(that)

  def startsWith(that:AbsolutePathSpec):Boolean = elements startsWith (that elements)
}
sealed trait RelativePathSpec extends PathSpec {
  override def /(that:RelativePathSpec):RelativePathSpec = that match {
    case element:PathElementSpec ⇒ CompositeRelativePathSpec(this, element)
    case CompositeRelativePathSpec(childsHead, childsTail) ⇒ CompositeRelativePathSpec(this/childsHead, childsTail)
  }
  override def /(that:String):RelativePathSpec = this/PathElementSpec(that)

  def startsWith(that:RelativePathSpec):Boolean = elements startsWith that.elements
}

final case class PathElementSpec(name:String) extends RelativePathSpec {
  require(name nonEmpty, s"Path element '$name' cannot be empty.")
  require(name.trim == name, s"Path element '$name' cannot start or end with white space.")
  require(name.indexOf('/') == -1, s"Path element '$name' cannot contain a slash.")
  require(name != ".", s"'.' is not a valid path element.")
  require(name != "..", s"'..' is not a valid path element.")
  if(name.length > 1) require(!(name startsWith "$"), s"Path element '$name' cannot start with a dollar sign.")

  override def toString = name
  private[path] def elements = Stream(this)
}
final case class CompositeRelativePathSpec(parent:RelativePathSpec, child:PathElementSpec) extends RelativePathSpec {
  private[path] def elements = parent.elements :+ child
  override def toString = s"$parent/$child"
}

final case class CompositeAbsolutePathSpec(parent:AbsolutePathSpec, child:PathElementSpec) extends AbsolutePathSpec {
  private[path] def elements = parent.elements :+ child
  override def toString = s"$parent/$child"
}
sealed trait AbsolutePathBase extends AbsolutePathSpec {
  private[path] def elements = Stream(this)
}
object UserHomePathSpec extends AbsolutePathBase {override def toString = "$HOME"}
