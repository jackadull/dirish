package net.jackadull.dirish.workflow.locking

import net.jackadull.dirish.op.Op.ProxyOp
import net.jackadull.dirish.op.io.DeleteFile
import net.jackadull.dirish.op.settings.LockFilePath
import net.jackadull.dirish.op.{Op, OpError}

import scala.language.higherKinds

object FreeLock extends ProxyOp[Unit,OpError,LockingStyle] {
  protected val innerOp:Op[Unit,OpError,LockingStyle] = LockFilePath.get >> DeleteFile
}
