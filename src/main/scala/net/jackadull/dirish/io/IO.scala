package net.jackadull.dirish.io

import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.higherKinds

/** IO Monad-like with IO specific methods for IO operations. Such operations are considered to be deferred and will
 * by default not be executed. Only when `markForExecution` is called in a tail value, all operations before and up to
 * that tail element will be executed asynchronously, in that order.
 *
 * The caller has no way to block until execution is finished. However, using `map`, the caller can introduce a
 * callback into the chain. */
trait IO[I[+_]] {
  def markForExecution[A](a:I[A]):I[Unit]

  def aggregate[A,B](as:Traversable[I[A]])(z: ⇒B)(seqop:(B,A)⇒B, combop:(B,B)⇒B):I[B]
  def bind[A](a: ⇒A):I[A]
  def flatMap[A,B](a:I[A])(f:A⇒I[B]):I[B]
  def flatten[A](a:I[I[A]]):I[A]
  def map[A,B](a:I[A])(f:A⇒B):I[B]

  def addGitModuleRemote(gitModulePath:AbsolutePathSpec, remoteName:String, remoteURI:String):I[AddGitModuleRemoteResult]
  def cloneGitModule(gitModulePath:AbsolutePathSpec, remoteName:String, remoteURI:String):I[CloneGitModuleResult]
  def createDirectory(directoryPath:AbsolutePathSpec):I[CreateDirectoryResult]
  def getFileInfo(path:AbsolutePathSpec):I[GetFileInfoResult]
  def hasLocalGitChanges(gitModulePath:AbsolutePathSpec):I[HasLocalGitChangesResult]
  def listDirectoryContents(directoryPath:AbsolutePathSpec):I[ListDirectoryContentsResult]
  def log(category:LogCategory, message:String, throwableOpt:Option[Throwable]=None):I[LogResult]
  def moveFile(sourcePath:AbsolutePathSpec, targetPath:AbsolutePathSpec):I[MoveFileResult]
  def moveToTrash(path:AbsolutePathSpec):I[MoveToTrashResult]
  def removeGitModule(gitModulePath:AbsolutePathSpec):I[RemoveGitModuleResult]
  def removeGitModuleRemote(gitModulePath:AbsolutePathSpec, remoteName:String):I[RemoveGitModuleRemoteResult]

  def isDirectoryEmptyEnoughAsMoveTarget(directoryPath:AbsolutePathSpec):I[IsDirectoryEmptyEnoughAsMoveTargetResult] =
    map(isDirectoryEmptyEnoughForRemoving(directoryPath)) {case r:BooleanIOResult ⇒ r; case r:GenericIOError ⇒ r}
  def isDirectoryEmptyEnoughForRemoving(directoryPath:AbsolutePathSpec):I[IsDirectoryEmptyEnoughForRemovingResult] =
    flatMap(getFileInfo(directoryPath)) {
      case FileInfoResult(DirectoryFileInfo(_)) ⇒ flatMap(listDirectoryContents(directoryPath)) {
        case DirectoryListResult(dirList) ⇒
          def seqop(a:I[Boolean], b:Boolean):I[Boolean] = map(a) {_ && b}
          def combo(a:I[Boolean], b:I[Boolean]):I[Boolean] = flatMap(a) {ba ⇒ map(b) {bb ⇒ ba && bb}}
          map(flatten(aggregate(dirList.toSeq map {
            case DirectoryFileInfo(dsStore) if dsStore hasLastElement ".DS_Store" ⇒ bind(true)
            case DirectoryFileInfo(childDir) ⇒ map(isDirectoryEmptyEnoughForRemoving(childDir)) {
              case BooleanIOResult(true) ⇒ true
              case _ ⇒ false
            }
            case _ ⇒ bind(false)
          })(bind(true))(seqop, combo)))(BooleanIOResult)
        case DirectoryNotFound ⇒ bind(BooleanIOResult(true))
        case err:GenericIOError ⇒ bind(err)
      }
      case FileInfoResult(NonExistingFileInfo(_)) ⇒ bind(BooleanIOResult(true))
      case FileInfoResult(_) ⇒ bind(BooleanIOResult(false))
      case err:GenericIOError ⇒ bind(err)
    }
}
