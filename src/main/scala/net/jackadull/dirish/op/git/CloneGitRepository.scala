package net.jackadull.dirish.op.git

import net.jackadull.dirish.op.Op
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

final case class CloneGitRepository(path:AbsolutePathSpec, remoteName:String, uri:String) extends Op[Unit,GenericGitError,GitStyle] {
  def instantiateIn[V[+_,+_]](style:GitStyle[V]):V[Unit,GenericGitError] =
    style cloneGitRepository (path, remoteName, uri)
}
