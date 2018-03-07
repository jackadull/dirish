package net.jackadull.dirish.op.git

import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

trait GitStyleProxy[V[+_,+_]] extends GitStyle[V] {
  protected def gitStyle:GitStyle[V]

  def addGitRemote(path:AbsolutePathSpec, remoteName:String, uri:String):V[Unit,GenericGitError] = gitStyle addGitRemote (path, remoteName, uri)
  def cloneGitRepository(path:AbsolutePathSpec, remoteName:String, uri:String):V[Unit,GenericGitError] = gitStyle cloneGitRepository (path, remoteName, uri)
  def pullGitRepository(path:AbsolutePathSpec):V[Unit,GenericGitError] = gitStyle pullGitRepository path
  def hasLocalGitChanges(path:AbsolutePathSpec):V[Boolean,GenericGitError] = gitStyle hasLocalGitChanges path
  def removeGitRemote(path:AbsolutePathSpec, remoteName:String):V[Unit,GenericGitError] = gitStyle removeGitRemote (path, remoteName)
  def removeGitRepository(path:AbsolutePathSpec):V[Unit,GenericGitError] = gitStyle removeGitRepository path
}
