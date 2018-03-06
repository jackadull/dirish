package net.jackadull.dirish.migration.stage

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationResult
import net.jackadull.dirish.migration.step.{MigrationStep, MoveProjectStep}
import net.jackadull.dirish.model.{ProjectMovedSpec, ProjectsMovedStage}
import net.jackadull.dirish.op.Op

import scala.language.postfixOps

private[stage] case class ProjectsMovedMigrationStage(stage:ProjectsMovedStage, soFar:Op[MigrationResult,Nothing,MigrationStyle])
extends MigrationStage[ProjectsMovedStage,ProjectMovedSpec] {
  protected def changes:Traversable[ProjectMovedSpec] = stage.projectsMoved
  protected def nextStage(thisResult:Op[MigrationResult,Nothing,MigrationStyle]):Op[MigrationResult,Nothing,MigrationStyle] =
    BaseDirectoriesMovedMigrationStage(stage nextStage, thisResult)
  protected def step(change:ProjectMovedSpec):MigrationStep = MoveProjectStep(change)
}
