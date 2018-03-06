package net.jackadull.dirish.op.git

import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

trait GitStyle[V[+_,+_]] {
  def addGitRemote(path:AbsolutePathSpec, remoteName:String, uri:String):V[Unit,GenericGitError]
  def cloneGitRepository(path:AbsolutePathSpec, remoteName:String, uri:String):V[Unit,GenericGitError]
  def hasLocalGitChanges(path:AbsolutePathSpec):V[Boolean,GenericGitError]
  def removeGitRemote(path:AbsolutePathSpec, remoteName:String):V[Unit,GenericGitError]
  def removeGitRepository(path:AbsolutePathSpec):V[Unit,GenericGitError]
}
