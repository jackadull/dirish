package net.jackadull.dirish.migration.stage

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationResult
import net.jackadull.dirish.migration.step.{MigrationStep, RemoveBaseDirectoryStep}
import net.jackadull.dirish.model.{BaseDirectoriesRemovedStage, BaseDirectoryRemovedSpec}
import net.jackadull.dirish.op.Op

import scala.language.postfixOps

private[stage] final case class BaseDirectoriesRemovedMigrationStage(stage:BaseDirectoriesRemovedStage, soFar:Op[MigrationResult,Nothing,MigrationStyle])
extends MigrationStage[BaseDirectoriesRemovedStage,BaseDirectoryRemovedSpec] {
  protected def changes:Traversable[BaseDirectoryRemovedSpec] = stage.baseDirectoriesRemoved
  protected def nextStage(thisResult:Op[MigrationResult,Nothing,MigrationStyle]):Op[MigrationResult,Nothing,MigrationStyle] =
    GitFirstRemotesChangedMigrationStage(stage nextStage, thisResult)
  protected def step(change:BaseDirectoryRemovedSpec):MigrationStep = RemoveBaseDirectoryStep(change)
}
