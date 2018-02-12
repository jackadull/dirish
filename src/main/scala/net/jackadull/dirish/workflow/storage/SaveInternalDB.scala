package net.jackadull.dirish.workflow.storage

import java.nio.charset.StandardCharsets.UTF_8

import net.jackadull.dirish.io.IODSL._
import net.jackadull.dirish.io._
import net.jackadull.dirish.marshalling.{ProjectConfigToToken, RenderProjectConfig}
import net.jackadull.dirish.model.ProjectConfig
import net.jackadull.dirish.path.{AbsolutePathSpec, CompositeAbsolutePathSpec}

import scala.language.higherKinds

final case class SaveInternalDB(db:ProjectConfig) extends IOOp[SaveInternalDBResult] {
  def instantiate[I[+_]](io:IO[I]):I[SaveInternalDBResult] = internal instantiate io

  private val internal:IOOp[SaveInternalDBResult] = ParameterValue(InternalDBFilePath) flatMap {path =>
    IOSeq(Seq(ensureParentExists(path), writeFile(path))) map {
      case IOSuccess ⇒ InternalDBSaved
      case err:IOError ⇒ CannotSaveInternalDB(err)
      case anythingElse ⇒ CannotSaveInternalDB(CustomIOError(s"Unexpected IO result: $anythingElse"))
    }
  }

  private def ensureParentExists(path:AbsolutePathSpec):IOOp[IOResult] = path match {
    case CompositeAbsolutePathSpec(parent, _) ⇒ GetFileInfo(parent) flatMap {
      case FileInfoResult(_:DirectoryFileInfo) ⇒ IOBind(IOSuccess)
      case FileInfoResult(_:NonExistingFileInfo) ⇒ CreateDirectory(parent)
      case FileInfoResult(_) ⇒ IOBind(CustomIOError(s"Internal DB file parent '$parent' is not a directory."))
      case err:IOError ⇒ IOBind(err)
    }
    case _ ⇒ IOBind(IOSuccess)
  }

  private def writeFile(path:AbsolutePathSpec):IOOp[IOResult] =
    SaveStringToFile(path, RenderProjectConfig(ProjectConfigToToken(db)), UTF_8)
}

sealed trait SaveInternalDBResult
final case class CannotSaveInternalDB(error:IOError) extends SaveInternalDBResult
object InternalDBSaved extends SaveInternalDBResult
