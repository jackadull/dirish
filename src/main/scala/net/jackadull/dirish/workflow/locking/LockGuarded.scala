package net.jackadull.dirish.workflow.locking

import net.jackadull.dirish.io.IODSL.{IOBind, IOOp, Log}
import net.jackadull.dirish.io.LogCategory.{ExecutionFailure, NonCriticalExecutionFailure}
import net.jackadull.dirish.io._

import scala.language.higherKinds

final case class LockGuarded[+A>:IOResult](internal:IOOp[A]) extends IOOp[A] {
  def instantiate[I[+_]](io:IO[I]):I[A] = combined instantiate io

  private def combined:IOOp[A] = ObtainLock flatMap {
    case LockObtained ⇒ internal flatMap {internalResult ⇒
      FreeLock flatMap {
        case LockFreed ⇒ IOBind(internalResult)
        case LockFileNotFound ⇒ Log(NonCriticalExecutionFailure, "Tried to remove lock file, but lock file not found.") map {_ ⇒ internalResult}
        case CannotFreeLock(err@GenericIOError(cause)) ⇒ Log(ExecutionFailure, "Cannot remove lock file:", Some(cause)) map {_ ⇒ internalResult match {
          case internalError:IOError ⇒ internalError
          case _ ⇒ err
        }}
        case CannotFreeLock(err) ⇒ Log(ExecutionFailure, s"Cannot remove lock file: $err") map {_ ⇒ internalResult match {
          case internalError:IOError ⇒ internalError
          case _ ⇒ err
        }}
      }
    }
    case AlreadyLocked ⇒ Log(ExecutionFailure, "Lock file exists already.") map {_ ⇒ CustomIOError("Lock file exists already.")}
    case CannotObtainLock(err@GenericIOError(cause)) ⇒ Log(ExecutionFailure, "Cannot obtain lock:", Some(cause)) map {_ ⇒ err}
    case CannotObtainLock(err) ⇒ Log(ExecutionFailure, s"Cannot obtain lock: $err") map {_ ⇒ err}
  }
}
