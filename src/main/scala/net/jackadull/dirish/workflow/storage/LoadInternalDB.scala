package net.jackadull.dirish.workflow.storage

import net.jackadull.dirish.model.ProjectConfig
import net.jackadull.dirish.op.Op.ProxyOp
import net.jackadull.dirish.op.settings.InternalDBFilePath
import net.jackadull.dirish.op.{Op, OpError}

import scala.language.postfixOps

object LoadInternalDB extends ProxyOp[ProjectConfig,OpError,StorageStyle] {
  protected val innerOp:Op[ProjectConfig,OpError,StorageStyle] =
    LoadConfigFile(InternalDBFilePath, emptyIfNonExistent=true)
}
