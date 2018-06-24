package net.jackadull.dirish.migration.step

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationLogFormat.MLVerb
import net.jackadull.dirish.migration.{MigrationLogFormat, MigrationResult}
import net.jackadull.dirish.model.ProjectMovedSpec
import net.jackadull.dirish.op.combinator.FailWith
import net.jackadull.dirish.op.io._
import net.jackadull.dirish.op.{Op, OpError}
import net.jackadull.dirish.path.CompositeAbsolutePathSpec

import scala.language.postfixOps

final case class MoveProjectStep(change:ProjectMovedSpec) extends MigrationStep {
  type C = ProjectMovedSpec
  protected def logFormat:MigrationLogFormat[ProjectMovedSpec] = new MigrationLogFormat[ProjectMovedSpec](MLVerb.Move, change) {
    def grammaticalObject(state:MigrationResult):String =
      s"project ${change id} from ${state.baseDirectoryPath(change.from.baseDirectoryID)/change.from.localPath} to ${state.baseDirectoryPath(change.to.baseDirectoryID)/change.to.localPath}"
  }
  protected def mainOp(state:MigrationResult):Op[Unit,OpError,MigrationStyle] = {
    val targetDirectory = state.baseDirectoryPath(change.to.baseDirectoryID) / change.to.localPath
    val moveOp:Op[Unit,OpError,MigrationStyle] = MoveFile(state.baseDirectoryPath(change.from.baseDirectoryID)/change.from.localPath, targetDirectory)
    targetDirectory match {
      case CompositeAbsolutePathSpec(targetParent, _) ⇒ FileKindInfo(targetParent) >> {
        case IsDirectory ⇒ moveOp
        case IsNonExistent ⇒ CreateDirectories(targetParent) ~> moveOp
        case IsRegularFile ⇒ FailWith(NotADirectory(s"Cannot move because parent of target directory is a regular file", targetParent toString))
      }
      case _ ⇒ moveOp
    }
  }
}
