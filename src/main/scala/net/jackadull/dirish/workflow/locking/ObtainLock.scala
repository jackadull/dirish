package net.jackadull.dirish.workflow.locking

import net.jackadull.dirish.io.IODSL._
import net.jackadull.dirish.io._
import net.jackadull.dirish.path.{AbsolutePathSpec, CompositeAbsolutePathSpec}

import scala.language.higherKinds

object ObtainLock extends IOOp[ObtainLockResult] {
  def instantiate[I[+_]](io:IO[I]):I[ObtainLockResult] = io.flatMap(io parameterValue LockFilePath) {lockFilePath ⇒
    op(lockFilePath) instantiate io
  }

  private def op(lockFilePath:AbsolutePathSpec):IOOp[ObtainLockResult] =
    IOSeq(Seq(ensureLockFileParentExists(lockFilePath), createLockFile(lockFilePath))) map {
      case IOSuccess ⇒ LockObtained
      case TargetFileAlreadyExists ⇒ AlreadyLocked
      case err:IOError ⇒ CannotObtainLock(err)
      case unexpected ⇒ CannotObtainLock(CustomIOError(s"Unexpected IO result: $unexpected"))
    }

  private def ensureLockFileParentExists(lockFilePath:AbsolutePathSpec):IOOp[IOResult] = lockFilePath match {
    case CompositeAbsolutePathSpec(lockFileParent, _) ⇒ GetFileInfo(lockFileParent) flatMap {
      case FileInfoResult(_:DirectoryFileInfo) ⇒ IOBind(IOSuccess)
      case FileInfoResult(_:NonExistingFileInfo) ⇒ CreateDirectory(lockFileParent)
      case FileInfoResult(_) ⇒ IOBind(CustomIOError(s"Lock file parent '$lockFileParent' is not a directory."))
      case err:IOError ⇒ IOBind(err)
    }
    case _ ⇒ IOBind(IOSuccess)
  }

  private def createLockFile(lockFilePath:AbsolutePathSpec):IOOp[CreateLockFileResult] = CreateLockFile(lockFilePath)
}

sealed trait ObtainLockResult
object AlreadyLocked extends ObtainLockResult
final case class CannotObtainLock(reason:IOError) extends ObtainLockResult
object LockObtained extends ObtainLockResult
