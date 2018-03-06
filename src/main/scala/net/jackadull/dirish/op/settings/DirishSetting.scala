package net.jackadull.dirish.op.settings

import net.jackadull.dirish.path.AbsolutePathSpec

/** A value that is transported via the setting style. */
sealed trait DirishSetting[A] {
  val get:GetSetting[A] = GetSetting(this)
  def ident(v:A):A = v
}

object ApplicationDataDirectoryPath extends DirishSetting[AbsolutePathSpec]
object InternalDBFilePath extends DirishSetting[AbsolutePathSpec]
object LockFilePath extends DirishSetting[AbsolutePathSpec]
object UserConfigPath extends DirishSetting[AbsolutePathSpec]
