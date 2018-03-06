package net.jackadull.dirish.migration.step

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationLogFormat.MLVerb
import net.jackadull.dirish.migration.{MigrationLogFormat, MigrationResult}
import net.jackadull.dirish.model.ProjectActiveSignalRemovedSpec
import net.jackadull.dirish.op.combinator.ResultIn
import net.jackadull.dirish.op.{Op, OpError}

import scala.language.postfixOps

final case class RemoveProjectActiveSignalStep(change:ProjectActiveSignalRemovedSpec) extends MigrationStep {
  type C = ProjectActiveSignalRemovedSpec
  protected def logFormat:MigrationLogFormat[ProjectActiveSignalRemovedSpec] = new MigrationLogFormat[ProjectActiveSignalRemovedSpec](MLVerb.Remove, change) {
    def grammaticalObject(state:MigrationResult):String =
      s"active signal '${change signal}' from project ${change projectID} at ${state absoluteProjectPath (change projectID)}"
  }
  protected def mainOp(state:MigrationResult):Op[Unit,OpError,MigrationStyle] = ResultIn success
}
