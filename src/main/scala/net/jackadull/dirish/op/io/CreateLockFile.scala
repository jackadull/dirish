package net.jackadull.dirish.op.io

import net.jackadull.dirish.op.Op
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

final case class CreateLockFile(path:AbsolutePathSpec) extends Op[Unit,CreateLockFileError,IOStyle] {
  def instantiateIn[V[+_,+_]](style:IOStyle[V]):V[Unit,CreateLockFileError] = style createLockFile path
}
