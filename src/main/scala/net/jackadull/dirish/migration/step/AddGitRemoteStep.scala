package net.jackadull.dirish.migration.step

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationLogFormat.MLVerb
import net.jackadull.dirish.migration.{MigrationLogFormat, MigrationResult}
import net.jackadull.dirish.model.GitRemoteAddedSpec
import net.jackadull.dirish.op.git.AddGitRemote
import net.jackadull.dirish.op.{Op, OpError}

import scala.language.postfixOps

final case class AddGitRemoteStep(change:GitRemoteAddedSpec) extends MigrationStep {
  type C = GitRemoteAddedSpec
  protected def logFormat:MigrationLogFormat[GitRemoteAddedSpec] = new MigrationLogFormat[GitRemoteAddedSpec](MLVerb.Add, change) {
    def grammaticalObject(state:MigrationResult):String =
      s"Git remote '${change.remote _1}' to ${state absoluteProjectPath (change projectID)}"
  }
  protected def mainOp(state:MigrationResult):Op[Unit,OpError,MigrationStyle] =
    AddGitRemote(state absoluteProjectPath (change projectID), change.remote _1, change.remote _2)
}
