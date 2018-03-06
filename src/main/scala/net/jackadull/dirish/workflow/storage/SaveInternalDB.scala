package net.jackadull.dirish.workflow.storage

import java.nio.charset.StandardCharsets.UTF_8

import net.jackadull.dirish.marshalling.{ProjectConfigToToken, RenderProjectConfig}
import net.jackadull.dirish.model.ProjectConfig
import net.jackadull.dirish.op.Op.ProxyOp
import net.jackadull.dirish.op.combinator.{FailWith, ResultIn}
import net.jackadull.dirish.op.io._
import net.jackadull.dirish.op.settings.InternalDBFilePath
import net.jackadull.dirish.op.{Op, OpError}
import net.jackadull.dirish.path.{AbsolutePathSpec, CompositeAbsolutePathSpec}

import scala.language.postfixOps

final case class SaveInternalDB(db:ProjectConfig) extends ProxyOp[Unit,OpError,StorageStyle] {
  protected def innerOp:Op[Unit,OpError,StorageStyle] = InternalDBFilePath.get >>[Unit,OpError,StorageStyle] {path ⇒
    ensureParentDirectoryExists(path) ~> SaveStringToFile(path, RenderProjectConfig(ProjectConfigToToken(db)), UTF_8)}

  private def ensureParentDirectoryExists(path:AbsolutePathSpec):Op[Unit,IOError,StorageStyle] = path match {
    case CompositeAbsolutePathSpec(parent, _) ⇒ FileKindInfo(parent) >> {
      case IsNonExistent ⇒ CreateDirectories(parent)
      case IsDirectory ⇒ ResultIn success
      case IsRegularFile ⇒ FailWith(NotADirectory(s"Internal DB file parent is not a directory.", parent toString))
    }
    case _ ⇒ ResultIn success
  }
}
