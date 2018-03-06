package net.jackadull.dirish.migration.stage

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationResult
import net.jackadull.dirish.migration.stage.MigrationStage.SkipMaybe
import net.jackadull.dirish.migration.step.{MigrationStep, RemoveProjectActiveSignalStep}
import net.jackadull.dirish.model.{ProjectActiveSignalRemovedSpec, ProjectActiveSignalsRemovedStage}
import net.jackadull.dirish.op.Op

import scala.language.postfixOps

private[stage] final case class ProjectActiveSignalsRemovedMigrationStage(stage:ProjectActiveSignalsRemovedStage, soFar:Op[MigrationResult,Nothing,MigrationStyle])
extends MigrationStage[ProjectActiveSignalsRemovedStage,ProjectActiveSignalRemovedSpec] {
  override protected def changeSkippingPolicy:MigrationStage.ChangeSkippingPolicy[ProjectActiveSignalRemovedSpec] = new SkipMaybe[ProjectActiveSignalRemovedSpec] {
    def shouldSkip(change:ProjectActiveSignalRemovedSpec):Boolean = stage willProjectBeRemoved (change projectID)
    def skipMessageSingular(skipped:ProjectActiveSignalRemovedSpec, state:MigrationResult):String =
      s"Skipped removing project active signal from project ${skipped projectID} because project is scheduled for removal."
    def skipMessagePlural(skipped:Traversable[ProjectActiveSignalRemovedSpec], state:MigrationResult):String =
      s"Skipped removing project active signals because their projects are scheduled for removal: ${skipped.map(_.projectID.toString).toSeq.sorted mkString ", "}"
  }

  protected def changes:Traversable[ProjectActiveSignalRemovedSpec] = stage.projectActiveSignalsRemoved
  protected def nextStage(thisResult:Op[MigrationResult,Nothing,MigrationStyle]):Op[MigrationResult,Nothing,MigrationStyle] =
    ProjectsRemovedMigrationStage(stage nextStage, thisResult)
  protected def step(change:ProjectActiveSignalRemovedSpec):MigrationStep = RemoveProjectActiveSignalStep(change)
}
