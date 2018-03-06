package net.jackadull.dirish.migration.stage

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationResult
import net.jackadull.dirish.migration.step.{AddProjectStep, MigrationStep}
import net.jackadull.dirish.model.{ProjectAddedSpec, ProjectsAddedStage}
import net.jackadull.dirish.op.Op

import scala.language.postfixOps

private[stage] final case class ProjectsAddedMigrationStage(stage:ProjectsAddedStage, soFar:Op[MigrationResult,Nothing,MigrationStyle])
extends MigrationStage[ProjectsAddedStage,ProjectAddedSpec] {
  protected def changes:Traversable[ProjectAddedSpec] = stage.projectsAdded
  protected def nextStage(thisResult:Op[MigrationResult,Nothing,MigrationStyle]):Op[MigrationResult,Nothing,MigrationStyle] =
    ProjectActiveSignalsAddedMigrationStage(stage nextStage, thisResult)
  protected def step(change:ProjectAddedSpec):MigrationStep = AddProjectStep(change)
}
