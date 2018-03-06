package net.jackadull.dirish.migration.step

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationLogFormat.MLVerb
import net.jackadull.dirish.migration.{MigrationLogFormat, MigrationResult}
import net.jackadull.dirish.model.BaseDirectoryMovedSpec
import net.jackadull.dirish.op.io.MoveFile
import net.jackadull.dirish.op.{Op, OpError}

import scala.language.postfixOps

final case class MoveBaseDirectoryStep(change:BaseDirectoryMovedSpec) extends MigrationStep {
  type C = BaseDirectoryMovedSpec
  protected def logFormat:MigrationLogFormat[BaseDirectoryMovedSpec] = new MigrationLogFormat[BaseDirectoryMovedSpec](MLVerb.Move, change) {
    def grammaticalObject(state:MigrationResult):String =
      s"base directory ${change id} from ${change from} to ${change to}"
  }
  protected def mainOp(state:MigrationResult):Op[Unit,OpError,MigrationStyle] =
    MoveFile(change from, change to)
}
