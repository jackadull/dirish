package net.jackadull.dirish.op.git

import net.jackadull.dirish.op.Op
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

final case class RemoveGitRemote(path:AbsolutePathSpec, remoteName:String) extends Op[Unit,GenericGitError,GitStyle] {
  def instantiateIn[V[+_,+_]](style:GitStyle[V]):V[Unit,GenericGitError] = style removeGitRemote (path, remoteName)
}
