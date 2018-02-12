package net.jackadull.dirish.io

import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.{higherKinds, postfixOps}

/** A value or setting that is obtained and transported via `IO`. */
sealed trait IOParameter[A] {
  def ev(a:A):A = a
}

object ApplicationDataDirectoryPath extends IOParameter[AbsolutePathSpec]
object InternalDBFilePath extends IOParameter[AbsolutePathSpec]
object LockFilePath extends IOParameter[AbsolutePathSpec]
object UserConfigPath extends IOParameter[AbsolutePathSpec]
