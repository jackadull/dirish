package net.jackadull.dirish.migration.stage

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationResult
import net.jackadull.dirish.migration.step.{AddBaseDirectoryStep, MigrationStep}
import net.jackadull.dirish.model.{BaseDirectoriesAddedStage, BaseDirectoryAddedSpec}
import net.jackadull.dirish.op.Op

import scala.language.postfixOps

private[stage] final case class BaseDirectoriesAddedMigrationStage(stage:BaseDirectoriesAddedStage, soFar:Op[MigrationResult,Nothing,MigrationStyle])
extends MigrationStage[BaseDirectoriesAddedStage,BaseDirectoryAddedSpec] {
  protected def changes:Traversable[BaseDirectoryAddedSpec] = stage.baseDirectoriesAdded
  protected def nextStage(thisResult:Op[MigrationResult,Nothing,MigrationStyle]):Op[MigrationResult,Nothing,MigrationStyle] =
    ProjectsAddedMigrationStage(stage nextStage, thisResult)
  protected def step(change:BaseDirectoryAddedSpec):MigrationStep = AddBaseDirectoryStep(change)
}
