package net.jackadull.dirish.op.io

import net.jackadull.dirish.op.Op
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

final case class ListDirectory(path:AbsolutePathSpec) extends Op[Set[AbsolutePathSpec],ListDirectoryError,IOStyle] {
  def instantiateIn[V[+_,+_]](style:IOStyle[V]):V[Set[AbsolutePathSpec],ListDirectoryError] = style listDirectory path
}
