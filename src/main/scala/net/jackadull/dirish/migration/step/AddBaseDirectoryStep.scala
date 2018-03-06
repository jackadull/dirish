package net.jackadull.dirish.migration.step

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationLogFormat.MLVerb
import net.jackadull.dirish.migration.{MigrationLogFormat, MigrationResult}
import net.jackadull.dirish.model.BaseDirectoryAddedSpec
import net.jackadull.dirish.op.io.CreateDirectories
import net.jackadull.dirish.op.{Op, OpError}

import scala.language.postfixOps

final case class AddBaseDirectoryStep(change:BaseDirectoryAddedSpec) extends MigrationStep {
  type C = BaseDirectoryAddedSpec
  protected def logFormat:MigrationLogFormat[BaseDirectoryAddedSpec] = new MigrationLogFormat[BaseDirectoryAddedSpec](MLVerb.Add, change) {
    def grammaticalObject(state:MigrationResult):String = s"base directory ${change id} at ${change path}"
  }
  protected def mainOp(state:MigrationResult):Op[Unit,OpError,MigrationStyle] = CreateDirectories(change path)
}
