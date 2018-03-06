package net.jackadull.dirish.migration.stage

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationResult
import net.jackadull.dirish.migration.step.{MigrationStep, MoveBaseDirectoryStep}
import net.jackadull.dirish.model.{BaseDirectoriesMovedStage, BaseDirectoryMovedSpec}
import net.jackadull.dirish.op.Op

import scala.language.postfixOps

private[stage] final case class BaseDirectoriesMovedMigrationStage(stage:BaseDirectoriesMovedStage, soFar:Op[MigrationResult,Nothing,MigrationStyle])
extends MigrationStage[BaseDirectoriesMovedStage,BaseDirectoryMovedSpec] {
  protected def changes:Traversable[BaseDirectoryMovedSpec] = stage.baseDirectoriesMoved
  protected def nextStage(thisResult:Op[MigrationResult,Nothing,MigrationStyle]):Op[MigrationResult,Nothing,MigrationStyle] =
    BaseDirectoriesAddedMigrationStage(stage nextStage, thisResult)
  protected def step(change:BaseDirectoryMovedSpec):MigrationStep = MoveBaseDirectoryStep(change)
}
