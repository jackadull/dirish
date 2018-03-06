package net.jackadull.dirish.migration.stage

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationResult
import net.jackadull.dirish.migration.stage.MigrationStage.SkipMaybe
import net.jackadull.dirish.migration.step.{MigrationStep, RemoveProjectStep}
import net.jackadull.dirish.model.{ProjectRemovedSpec, ProjectsRemovedStage}
import net.jackadull.dirish.op.Op

import scala.language.postfixOps

private[stage] final case class ProjectsRemovedMigrationStage(stage:ProjectsRemovedStage, soFar:Op[MigrationResult,Nothing,MigrationStyle])
extends MigrationStage[ProjectsRemovedStage,ProjectRemovedSpec] {
  override protected def changeSkippingPolicy:MigrationStage.ChangeSkippingPolicy[ProjectRemovedSpec] = new SkipMaybe[ProjectRemovedSpec] {
    def shouldSkip(change:ProjectRemovedSpec):Boolean = stage willBaseDirectoryBeRemoved (change.location baseDirectoryID)
    def skipMessageSingular(skipped:ProjectRemovedSpec, state:MigrationResult):String =
      s"Skipped deleting project ${skipped id} at ${state absoluteProjectPath (skipped id)} because its base directory is scheduled for removal."
    def skipMessagePlural(skipped:Traversable[ProjectRemovedSpec], state:MigrationResult):String =
      s"Skipped deleting projects because their base directory is scheduled for removal: ${skipped.map(_.id.toString).toSeq.sorted mkString ", "}"
  }

  protected def changes:Traversable[ProjectRemovedSpec] = stage.projectsRemoved
  protected def nextStage(thisResult:Op[MigrationResult,Nothing,MigrationStyle]):Op[MigrationResult,Nothing,MigrationStyle] =
    BaseDirectoriesRemovedMigrationStage(stage nextStage, thisResult)
  protected def step(change:ProjectRemovedSpec):MigrationStep = RemoveProjectStep(change)
}
