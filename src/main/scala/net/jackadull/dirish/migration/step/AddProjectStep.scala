package net.jackadull.dirish.migration.step

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationLogFormat.MLVerb
import net.jackadull.dirish.migration.{MigrationLogFormat, MigrationResult}
import net.jackadull.dirish.model.ProjectAddedSpec
import net.jackadull.dirish.op.io.CreateDirectories
import net.jackadull.dirish.op.{Op, OpError}

import scala.language.postfixOps

final case class AddProjectStep(change:ProjectAddedSpec) extends MigrationStep {
  type C = ProjectAddedSpec
  protected def logFormat:MigrationLogFormat[ProjectAddedSpec] = new MigrationLogFormat(MLVerb.Add, change) {
    def grammaticalObject(state:MigrationResult):String =
      s"project ${change id} at ${state.baseDirectoryPath(change.location.baseDirectoryID)/change.location.localPath}"
  }
  protected def mainOp(state:MigrationResult):Op[Unit,OpError,MigrationStyle] =
    CreateDirectories(state.baseDirectoryPath(change.location.baseDirectoryID)/change.location.localPath)
}
