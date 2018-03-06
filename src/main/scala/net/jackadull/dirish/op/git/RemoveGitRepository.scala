package net.jackadull.dirish.op.git

import net.jackadull.dirish.op.Op
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

/** Does not remove the target directory itself, but only the Git meta-information stored for it. The target directory
 * becomes just a regular directory, and won't be a Git repository any more. */
final case class RemoveGitRepository(path:AbsolutePathSpec) extends Op[Unit,GenericGitError,GitStyle] {
  def instantiateIn[V[+_,+_]](style:GitStyle[V]):V[Unit,GenericGitError] = style removeGitRepository path
}
