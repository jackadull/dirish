package net.jackadull.dirish.migration.step

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationLogFormat.MLVerb
import net.jackadull.dirish.migration.{MigrationLogFormat, MigrationResult}
import net.jackadull.dirish.model.GitRemoteRemovedSpec
import net.jackadull.dirish.op.git.RemoveGitRemote
import net.jackadull.dirish.op.{Op, OpError}

import scala.language.postfixOps

final case class RemoveGitRemoteStep(change:GitRemoteRemovedSpec) extends MigrationStep {
  type C = GitRemoteRemovedSpec
  protected def logFormat:MigrationLogFormat[GitRemoteRemovedSpec] = new MigrationLogFormat[GitRemoteRemovedSpec](MLVerb.Remove, change) {
    def grammaticalObject(state:MigrationResult):String =
      s"Git remote '${change removedRemoteName}' from project ${change projectID} at ${state absoluteProjectPath (change projectID)}"
  }
  protected def mainOp(state:MigrationResult):Op[Unit,OpError,MigrationStyle] =
    RemoveGitRemote(state absoluteProjectPath (change projectID), change removedRemoteName)
}
