package net.jackadull.dirish.migration.stage

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationResult
import net.jackadull.dirish.migration.step.{AddProjectActiveSignalStep, MigrationStep}
import net.jackadull.dirish.model.{ProjectActiveSignalAddedSpec, ProjectActiveSignalsAddedStage}
import net.jackadull.dirish.op.Op

import scala.language.postfixOps

private[stage] final case class ProjectActiveSignalsAddedMigrationStage(stage:ProjectActiveSignalsAddedStage, soFar:Op[MigrationResult,Nothing,MigrationStyle])
extends MigrationStage[ProjectActiveSignalsAddedStage,ProjectActiveSignalAddedSpec] {
  protected def changes:Traversable[ProjectActiveSignalAddedSpec] = stage.projectActiveSignalsAdded
  protected def nextStage(thisResult:Op[MigrationResult,Nothing,MigrationStyle]):Op[MigrationResult,Nothing,MigrationStyle] =
    GitRepositoriesAddedMigrationStage(stage nextStage, thisResult)
  protected def step(change:ProjectActiveSignalAddedSpec):MigrationStep = AddProjectActiveSignalStep(change)
}
