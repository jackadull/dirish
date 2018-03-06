package net.jackadull.dirish.workflow.locking

import net.jackadull.dirish.op.Op.ProxyOp
import net.jackadull.dirish.op.combinator.FailWith
import net.jackadull.dirish.op.io._
import net.jackadull.dirish.op.settings.LockFilePath
import net.jackadull.dirish.op.{Op, OpError}
import net.jackadull.dirish.path.CompositeAbsolutePathSpec

object ObtainLock extends ProxyOp[Unit,OpError,LockingStyle] {
  protected val innerOp:Op[Unit,OpError,LockingStyle] = LockFilePath.get >> {
    case p@CompositeAbsolutePathSpec(parent, _) ⇒ FileKindInfo(parent) >> {
      case IsNonExistent ⇒ CreateDirectories(parent) ~> CreateLockFile(p)
      case IsDirectory ⇒ CreateLockFile(p)
      case IsRegularFile ⇒ FailWith(NotADirectory(s"Lock file parent $parent is a regular file.", parent toString))
    }
    case p ⇒ CreateLockFile(p)
  }
}
