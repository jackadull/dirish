package net.jackadull.dirish.op.io

import net.jackadull.dirish.op.Op
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

final case class MoveFile(source:AbsolutePathSpec, target:AbsolutePathSpec) extends Op[Unit,MoveFileError,IOStyle] {
  def instantiateIn[V[+_,+_]](style:IOStyle[V]):V[Unit,MoveFileError] = style moveFile (source, target)
}
