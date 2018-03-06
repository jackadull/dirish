package net.jackadull.dirish.migration.step

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationLogFormat.MLVerb
import net.jackadull.dirish.migration.{MigrationLogFormat, MigrationResult}
import net.jackadull.dirish.model.ProjectActiveSignalAddedSpec
import net.jackadull.dirish.op.combinator.ResultIn
import net.jackadull.dirish.op.{Op, OpError}

import scala.language.postfixOps

final case class AddProjectActiveSignalStep(change:ProjectActiveSignalAddedSpec) extends MigrationStep {
  type C = ProjectActiveSignalAddedSpec
  protected def logFormat:MigrationLogFormat[ProjectActiveSignalAddedSpec] = new MigrationLogFormat(MLVerb.Add, change) {
    def grammaticalObject(state:MigrationResult):String =
      s"project active signal for project ${change projectID} at ${state absoluteProjectPath (change projectID)}"
  }
  protected def mainOp(state:MigrationResult):Op[Unit,OpError,MigrationStyle] = ResultIn success
}
