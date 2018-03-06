package net.jackadull.dirish.migration.step

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationLogFormat.MLVerb
import net.jackadull.dirish.migration.{MigrationLogFormat, MigrationResult}
import net.jackadull.dirish.model.{ConfigChangeSpec, GitRemoteRemovedSpec, GitRepositoryRemovedSpec}
import net.jackadull.dirish.op.git.RemoveGitRepository
import net.jackadull.dirish.op.{Op, OpError}

import scala.language.postfixOps

final case class RemoveGitRepositoryStep(change:GitRepositoryRemovedSpec) extends MigrationStep {
  type C = GitRepositoryRemovedSpec
  override protected def includesSkippedUpstreamChange(skipped:ConfigChangeSpec, state:MigrationResult):Boolean = skipped match {
    case GitRemoteRemovedSpec(pid, _) ⇒ pid == change.projectID
    case _ ⇒ false
  }
  protected def logFormat:MigrationLogFormat[GitRepositoryRemovedSpec] = new MigrationLogFormat[GitRepositoryRemovedSpec](MLVerb.Remove, change) {
    def grammaticalObject(state:MigrationResult):String =
      s"Git repository of project ${change projectID} at ${state absoluteProjectPath (change projectID)}"
  }
  protected def mainOp(state:MigrationResult):Op[Unit,OpError,MigrationStyle] =
    RemoveGitRepository(state absoluteProjectPath (change projectID))
}
