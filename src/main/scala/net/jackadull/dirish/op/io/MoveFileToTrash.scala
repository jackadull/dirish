package net.jackadull.dirish.op.io

import net.jackadull.dirish.op.Op
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

final case class MoveFileToTrash(path:AbsolutePathSpec) extends Op[Unit,MoveToTrashError,IOStyle] {
  def instantiateIn[V[+_,+_]](style:IOStyle[V]):V[Unit,MoveToTrashError] = style moveFileToTrash path
}
