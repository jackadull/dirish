package net.jackadull.dirish.op.git

import net.jackadull.dirish.op.Op
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

final case class HasLocalGitChanges(path:AbsolutePathSpec) extends Op[Boolean,GenericGitError,GitStyle] {
  def instantiateIn[V[+_,+_]](style:GitStyle[V]):V[Boolean,GenericGitError] = style hasLocalGitChanges path
}
