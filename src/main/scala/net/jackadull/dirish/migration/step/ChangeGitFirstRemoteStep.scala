package net.jackadull.dirish.migration.step

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationLogFormat.MLVerb
import net.jackadull.dirish.migration.{MigrationLogFormat, MigrationResult}
import net.jackadull.dirish.model.GitFirstRemoteChangedSpec
import net.jackadull.dirish.op.git.{AddGitRemote, RemoveGitRemote}
import net.jackadull.dirish.op.{Op, OpError}

import scala.language.postfixOps

final case class ChangeGitFirstRemoteStep(change:GitFirstRemoteChangedSpec) extends MigrationStep {
  type C = GitFirstRemoteChangedSpec
  protected def logFormat:MigrationLogFormat[GitFirstRemoteChangedSpec] = new MigrationLogFormat(MLVerb.Change, change) {
    def grammaticalObject(state:MigrationResult):String =
      s"first remote of project ${change projectID} at ${state absoluteProjectPath (change projectID)}"
  }
  protected def mainOp(state:MigrationResult):Op[Unit,OpError,MigrationStyle] =
    RemoveGitRemote(state absoluteProjectPath (change projectID), state firstRemoteNameOfProject (change projectID)) ~>
    AddGitRemote(state absoluteProjectPath (change projectID), change.newFirstRemote _1, change.newFirstRemote _2)
}
