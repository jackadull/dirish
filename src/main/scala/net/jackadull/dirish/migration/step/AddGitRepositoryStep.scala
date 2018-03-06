package net.jackadull.dirish.migration.step

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationLogFormat.MLVerb
import net.jackadull.dirish.migration.{MigrationLogFormat, MigrationResult}
import net.jackadull.dirish.model.GitRepositoryAddedSpec
import net.jackadull.dirish.op.git.CloneGitRepository
import net.jackadull.dirish.op.io.{CreateDirectories, FileKindInfo, IsNonExistent}
import net.jackadull.dirish.op.signals.Signal
import net.jackadull.dirish.op.{Op, OpError}
import net.jackadull.dirish.path.CompositeAbsolutePathSpec

import scala.language.postfixOps

final case class AddGitRepositoryStep(change:GitRepositoryAddedSpec) extends MigrationStep {
  type C = GitRepositoryAddedSpec
  override protected def signals(state:MigrationResult):Traversable[Signal[Boolean,OpError,MigrationStyle]] =
    state.state.activeSignals(change projectID)
  protected def logFormat:MigrationLogFormat[GitRepositoryAddedSpec] = new MigrationLogFormat[GitRepositoryAddedSpec](MLVerb.Clone, change, logDoing=true) {
    def grammaticalObject(state:MigrationResult):String =
      s"Git repository of project ${change projectID} into ${state absoluteProjectPath (change projectID)}"
  }
  protected def mainOp(state:MigrationResult):Op[Unit,OpError,MigrationStyle] = {
    val path = state.absoluteProjectPath(change projectID)
    val cloneOp = CloneGitRepository(path, change.firstRemote _1, change.firstRemote _2)
    path match {
      case CompositeAbsolutePathSpec(parent, _) ⇒ FileKindInfo(parent) >> {
        case IsNonExistent ⇒ CreateDirectories(parent) ~> cloneOp
        case _ ⇒ cloneOp
      }
      case _ ⇒ cloneOp
    }
  }
}
