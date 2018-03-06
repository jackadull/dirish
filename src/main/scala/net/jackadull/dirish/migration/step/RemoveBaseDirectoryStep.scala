package net.jackadull.dirish.migration.step

import java.util.UUID

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

final case class RemoveBaseDirectoryStep(change:BaseDirectoryRemovedSpec) extends MigrationStep {
  type C = BaseDirectoryRemovedSpec

  override protected def includesSkippedUpstreamChange(skipped:ConfigChangeSpec, state:MigrationResult):Boolean = skipped match {
    case GitRemoteRemovedSpec(pid, _) ⇒ state.state.projectBaseDirectoryID(pid) contains change.id
    case GitRepositoryRemovedSpec(pid) ⇒ state.state.projectBaseDirectoryID(pid) contains change.id
    case ProjectRemovedSpec(_, ProjectLocationSpec(bdid, _)) ⇒ bdid == change.id
    case _ ⇒ false
  }

  protected def logFormat:MigrationLogFormat[BaseDirectoryRemovedSpec] = new MigrationLogFormat[BaseDirectoryRemovedSpec](MLVerb.Move, change) {
    def grammaticalObject(state:MigrationResult):String = s"base directory ${change id} at ${change path} to trash"
  }

  protected def mainOp(state:MigrationResult):Op[Unit,OpError,MigrationStyle] = FileKindInfo(change path) >> {
    case IsNonExistent ⇒ ResultIn success
    case IsRegularFile ⇒ FailWith(NotADirectory("Found a regular file at the base directory's position", (change path) toString))
    case IsDirectory ⇒
      (validatedProjectPaths(state) >> {paths ⇒ ensureDoesNotContainUnmanagedFiles(paths, state)}) ~>
        MoveFileToTrash(state baseDirectoryPath (change id))
  }

  private def validatedProjectPaths(state:MigrationResult):Op[Set[AbsolutePathSpec],OpError,MigrationStyle] =
    ResultIn(state.projectIDsWithBaseDirectory(change id) map {validateProjectIsOkayForRemoval(_, state)}).
      foldLeft(Set[AbsolutePathSpec]()) {(paths:Set[AbsolutePathSpec], path:AbsolutePathSpec) ⇒ paths + path}

  private def validateProjectIsOkayForRemoval(projectID:UUID, state:MigrationResult):Op[AbsolutePathSpec,OpError,MigrationStyle] = {
    val projectPath = state absoluteProjectPath projectID
    if(state doesProjectHaveGitRepository projectID) HasLocalGitChanges(projectPath) >> {
      case true ⇒ FailWith(GenericMessageError(s"Project $projectID at $projectPath has local Git changes"))
      case false ⇒ ResultIn(projectPath)
    }
    else {
      def ensureIsEffectivelyEmpty(dir:AbsolutePathSpec):Op[Unit,OpError,MigrationStyle] = FileKindInfo(dir) >> {
        case IsNonExistent ⇒ FailWith(NoSuchFile("Path below base directory not found", dir toString))
        case IsDirectory ⇒ ListDirectory(dir) *>~ ensureIsEffectivelyEmpty
        case IsRegularFile ⇒ FailWith(DirectoryNotEmpty(s"Found a non-project file in project $projectID", dir toString))
      }
      ensureIsEffectivelyEmpty(projectPath) ~> ResultIn(projectPath)
    }
  }

  private def ensureDoesNotContainUnmanagedFiles(validatedProjectPaths:Set[AbsolutePathSpec], state:MigrationResult):Op[Unit,OpError,MigrationStyle] = {
    def recurse(p:AbsolutePathSpec):Op[Unit,OpError,MigrationStyle] =
      if(validatedProjectPaths(p)) ResultIn success
      else if(validatedProjectPaths exists (_ startsWith p)) FileKindInfo(p) >> {
        case IsNonExistent ⇒ ResultIn success
        case IsRegularFile ⇒ FailWith(GenericMessageError(s"Base directory contains non-project file: $p"))
        case IsDirectory ⇒ ListDirectory(p) *>~ recurse
      }
      else FailWith(GenericMessageError(s"Base directory contains non-project file: $p"))
    recurse(state baseDirectoryPath (change id))
  }
}
