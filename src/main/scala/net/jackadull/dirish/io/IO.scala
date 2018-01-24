package net.jackadull.dirish.io

import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

/** IO Monad-like with IO specific methods for IO operations. Such operations are considered to be deferred and will
 * by default not be executed. Only when `markForExecution` is called in a tail value, all operations before and up to
 * that tail element will be executed asynchronously, in that order.
 *
 * The caller has no way to block until execution is finished. However, using `map`, the caller can introduce a
 * callback into the chain. */
trait IO[I[_]] {
  def markForExecution[A](a:I[A]):I[Unit]

  def aggregate[A,B](as:Traversable[I[A]])(z: ⇒B)(seqop:(B,A)⇒B, combop:(B,B)⇒B):I[B]
  def map[A,B](a:I[A])(f:A⇒B):I[B]

  def addGitModuleRemote(gitModulePath:AbsolutePathSpec, remoteName:String, remoteURI:String):I[AddGitModuleRemoteResult]
  def cloneGitModule(gitModulePath:AbsolutePathSpec, remoteName:String, remoteURI:String):I[CloneGitModuleResult]
  def createDirectory(directoryPath:AbsolutePathSpec):I[CreateDirectoryResult]
  def getFileInfo(path:AbsolutePathSpec):I[GetFileInfoResult]
  def hasLocalGitChanges(gitModulePath:AbsolutePathSpec):I[HasLocalGitChangesResult]
  def isDirectoryEmptyEnoughAsMoveTarget(directoryPath:AbsolutePathSpec):I[IsDirectoryEmptyEnoughAsMoveTargetResult]
  def isDirectoryEmptyEnoughForRemoving(directoryPath:AbsolutePathSpec):I[IsDirectoryEmptyEnoughForRemovingResult]
  def listDirectoryContents(directoryPath:AbsolutePathSpec):I[ListDirectoryContentsResult]
  def moveFile(sourcePath:AbsolutePathSpec, targetPath:AbsolutePathSpec):I[MoveFileResult]
  def moveToTrash(path:AbsolutePathSpec):I[MoveToTrashResult]
  def removeGitModule(gitModulePath:AbsolutePathSpec):I[RemoveGitModuleResult]
  def removeGitModuleRemote(gitModulePath:AbsolutePathSpec, remoteName:String):I[RemoveGitModuleRemoteResult]
}
