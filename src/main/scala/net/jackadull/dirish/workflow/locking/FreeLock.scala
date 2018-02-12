package net.jackadull.dirish.workflow.locking

import net.jackadull.dirish.io.IODSL.IOOp
import net.jackadull.dirish.io._

import scala.language.higherKinds

object FreeLock extends IOOp[FreeLockResult] {
  def instantiate[I[+_]](io:IO[I]):I[FreeLockResult] = io.flatMap(io parameterValue LockFilePath) {lockFilePath ⇒
    io.map(io removeFile lockFilePath) {
      case IOSuccess ⇒ LockFreed
      case FileNotFound ⇒ LockFileNotFound
      case err:IOError ⇒ CannotFreeLock(err)
    }
  }
}

sealed trait FreeLockResult
final case class CannotFreeLock(error:IOError) extends FreeLockResult
object LockFileNotFound extends FreeLockResult
object LockFreed extends FreeLockResult
