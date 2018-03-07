package net.jackadull.dirish.op.git

import net.jackadull.dirish.op.{Op, OpError}
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

final case class PullGitRepository(path:AbsolutePathSpec) extends Op[Unit,OpError,GitStyle] {
  def instantiateIn[V[+_,+_]](style:GitStyle[V]):V[Unit,OpError] = style pullGitRepository path
}
