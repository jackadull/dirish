package net.jackadull.dirish.io

sealed trait IOResult
sealed trait IOError extends IOResult {
  def throwableOpt:Option[Throwable] = None
}
trait CustomIOResult extends IOResult

sealed trait AddGitModuleRemoteResult extends IOResult
sealed trait CloneGitModuleResult extends IOResult
sealed trait CreateDirectoryResult extends IOResult
sealed trait CreateLockFileResult extends IOResult
sealed trait GetFileInfoResult extends IOResult
sealed trait HasLocalGitChangesResult extends IOResult
sealed trait IsDirectoryEmptyEnoughAsMoveTargetResult extends IOResult
sealed trait IsDirectoryEmptyEnoughForRemovingResult extends IOResult
sealed trait ListDirectoryContentsResult extends IOResult
sealed trait LogResult extends IOResult
sealed trait MoveFileResult extends IOResult
sealed trait MoveToTrashResult extends IOResult
sealed trait ReadFileAsStringResult extends IOResult
sealed trait RemoveFileResult extends IOResult
sealed trait RemoveGitModuleResult extends IOResult
sealed trait RemoveGitModuleRemoteResult extends IOResult
sealed trait SaveStringToFileResult extends IOResult

final case class CustomIOError(message:String) extends IOError {
  override def toString:String = message
}
final case class GenericIOError(exception:Throwable) extends IOError with AddGitModuleRemoteResult
  with CloneGitModuleResult with CreateLockFileResult with CreateDirectoryResult with GetFileInfoResult
  with HasLocalGitChangesResult with IsDirectoryEmptyEnoughAsMoveTargetResult
  with IsDirectoryEmptyEnoughForRemovingResult with ListDirectoryContentsResult with MoveFileResult
  with MoveToTrashResult with ReadFileAsStringResult with RemoveFileResult with RemoveGitModuleResult
  with RemoveGitModuleRemoteResult with SaveStringToFileResult {
  override def throwableOpt:Option[Throwable] = Some(exception)
}
object IOSuccess extends AddGitModuleRemoteResult with CloneGitModuleResult with CreateDirectoryResult
  with CreateLockFileResult with LogResult with MoveFileResult with MoveToTrashResult with RemoveFileResult
  with RemoveGitModuleRemoteResult with RemoveGitModuleResult with SaveStringToFileResult

final case class BooleanIOResult(value:Boolean) extends HasLocalGitChangesResult
  with IsDirectoryEmptyEnoughAsMoveTargetResult with IsDirectoryEmptyEnoughForRemovingResult
final case class DirectoryListResult(containedFiles:Set[ExistingFileInfo]) extends ListDirectoryContentsResult
final case class FileInfoResult(fileInfo:FileInfo) extends GetFileInfoResult
final case class StringIOResult(value:String) extends ReadFileAsStringResult

object DirectoryNotCreated extends IOError with CreateDirectoryResult
object DirectoryNotFound extends IOError with ListDirectoryContentsResult // TODO refactor to FileNotFound
object FileNotFound extends IOError with AddGitModuleRemoteResult with HasLocalGitChangesResult with MoveFileResult
  with MoveToTrashResult with ReadFileAsStringResult with RemoveFileResult with RemoveGitModuleRemoteResult
  with RemoveGitModuleResult
object GitModuleRemoteNotFound extends IOError with RemoveGitModuleRemoteResult
object MoveToTrashNotSupported extends IOError with MoveToTrashResult
object NonDirectoryFileExistsAtGivenPath extends IOError with CreateDirectoryResult
object NotAGitModule extends IOError with RemoveGitModuleResult
object NotMovedToTrash extends IOError with MoveToTrashResult
object TargetFileAlreadyExists extends IOError with CloneGitModuleResult with CreateLockFileResult with MoveFileResult
