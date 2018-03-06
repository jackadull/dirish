package net.jackadull.dirish.op.io

import net.jackadull.dirish.op.Op
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

final case class DeleteFile(path:AbsolutePathSpec) extends Op[Unit,DeleteFileError,IOStyle] {
  def instantiateIn[V[+_,+_]](style:IOStyle[V]):V[Unit,DeleteFileError] = style deleteFile path
}
