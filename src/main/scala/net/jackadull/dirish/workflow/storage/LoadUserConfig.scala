package net.jackadull.dirish.workflow.storage

import net.jackadull.dirish.model.ProjectConfig
import net.jackadull.dirish.op.Op.ProxyOp
import net.jackadull.dirish.op.settings.UserConfigPath
import net.jackadull.dirish.op.{Op, OpError}

object LoadUserConfig extends ProxyOp[ProjectConfig,OpError,StorageStyle] {
  protected val innerOp:Op[ProjectConfig,OpError,StorageStyle] = LoadConfigFile(UserConfigPath)
}
