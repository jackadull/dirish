package net.jackadull.dirish.migration.stage

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationResult
import net.jackadull.dirish.migration.stage.MigrationStage.{ChangeSkippingPolicy, SkipMaybe}
import net.jackadull.dirish.migration.step.{MigrationStep, RemoveGitRepositoryStep}
import net.jackadull.dirish.model.{GitRepositoriesRemovedStage, GitRepositoryRemovedSpec}
import net.jackadull.dirish.op.Op

import scala.language.postfixOps

private[migration] case class GitRepositoriesRemovedMigrationStage(stage:GitRepositoriesRemovedStage, soFar:Op[MigrationResult,Nothing,MigrationStyle])
extends MigrationStage[GitRepositoriesRemovedStage,GitRepositoryRemovedSpec] {
  override protected def changeSkippingPolicy:ChangeSkippingPolicy[GitRepositoryRemovedSpec] = new SkipMaybe[GitRepositoryRemovedSpec] {
    def shouldSkip(change:GitRepositoryRemovedSpec):Boolean = stage willProjectBeRemoved (change projectID)
    def skipMessageSingular(skipped:GitRepositoryRemovedSpec, state:MigrationResult):String =
      s"Skipped deleting Git repository of project ${skipped projectID} at ${state absoluteProjectPath (skipped projectID)} because the project is scheduled for removal."
    def skipMessagePlural(skipped:Traversable[GitRepositoryRemovedSpec], state:MigrationResult):String =
      s"Skipped deleting Git repositories because their projects are scheduled for removal: ${skipped.map(_.projectID.toString).toSeq.sorted mkString ", "}"
  }

  protected def changes:Traversable[GitRepositoryRemovedSpec] = stage.gitRepositoriesRemoved
  protected def nextStage(thisResult:Op[MigrationResult,Nothing,MigrationStyle]):Op[MigrationResult,Nothing,MigrationStyle] =
    ProjectActiveSignalsRemovedMigrationStage(stage nextStage, thisResult)
  protected def step(change:GitRepositoryRemovedSpec):MigrationStep = RemoveGitRepositoryStep(change)
}
