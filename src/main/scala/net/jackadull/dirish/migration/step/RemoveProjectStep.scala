package net.jackadull.dirish.migration.step

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationLogFormat.MLVerb
import net.jackadull.dirish.migration.{MigrationLogFormat, MigrationResult}
import net.jackadull.dirish.model._
import net.jackadull.dirish.op.combinator.{FailWith, ResultIn}
import net.jackadull.dirish.op.git.HasLocalGitChanges
import net.jackadull.dirish.op.io._
import net.jackadull.dirish.op.{GenericMessageError, Op, OpError}
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.postfixOps

final case class RemoveProjectStep(change:ProjectRemovedSpec) extends MigrationStep {
  type C = ProjectRemovedSpec
  override protected def includesSkippedUpstreamChange(skipped:ConfigChangeSpec, state:MigrationResult) = skipped match {
    case GitRemoteRemovedSpec(pid, _) ⇒ pid == change.id
    case GitRepositoryRemovedSpec(pid) ⇒ pid == change.id
    case ProjectActiveSignalRemovedSpec(pid, _) ⇒ pid == change.id
    case _ ⇒ false
  }
  protected def logFormat:MigrationLogFormat[ProjectRemovedSpec] = new MigrationLogFormat[ProjectRemovedSpec](MLVerb.Move, change) {
    def grammaticalObject(state:MigrationResult):String =
      s"project ${change id} at ${state absoluteProjectPath (change id)} to trash"
  }
  protected def mainOp(state:MigrationResult):Op[Unit,OpError,MigrationStyle] = {
    val projectPath = state absoluteProjectPath (change id)
    if(state doesProjectHaveGitRepository (change id)) HasLocalGitChanges(projectPath) >> {
      case false ⇒ MoveFileToTrash(projectPath)
      case true ⇒ FailWith(GenericMessageError("Project has local Git changes."))
    } else {
      def ensureIsEffectivelyEmpty(dir:AbsolutePathSpec):Op[Unit,OpError,MigrationStyle] = FileKindInfo(dir) >> {
        case IsNonExistent ⇒ FailWith(NoSuchFile("Path below project directory not found", dir toString))
        case IsDirectory ⇒ ListDirectory(dir) *>~ ensureIsEffectivelyEmpty
        case IsRegularFile ⇒ FailWith(DirectoryNotEmpty(s"Found a non-project file in project ${change id}", dir toString))
      }
      ensureIsEffectivelyEmpty(projectPath) ~> ResultIn.success
    }
  }
}
