package net.jackadull.dirish.migration.step

import net.jackadull.dirish.model.DirishModel
import net.jackadull.dirish.model.environment.Environment

/** A command that can be executed on an environment, and that changes one [[net.jackadull.dirish.model.DirishModel]],
  * returning the changed version. */
trait MigrationStep extends ((DirishModel,Environment)⇒Either[(Exception,Option[DirishModel]),DirishModel]) {
  // TODO
}
