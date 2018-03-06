package net.jackadull.dirish.op.io

import net.jackadull.dirish.op.Op
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

final case class CreateDirectories(path:AbsolutePathSpec) extends Op[Unit,CreateDirectoryError,IOStyle] {
  def instantiateIn[V[+_,+_]](style:IOStyle[V]):V[Unit,CreateDirectoryError] = style createDirectories path
}
