package net.jackadull.dirish.migration.stage

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationResult
import net.jackadull.dirish.migration.stage.MigrationStage.{ChangeSkippingPolicy, SkipMaybe}
import net.jackadull.dirish.migration.step.{MigrationStep, RemoveGitRemoteStep}
import net.jackadull.dirish.model.{GitRemoteRemovedSpec, GitRemotesRemovedStage}
import net.jackadull.dirish.op.Op

import scala.language.postfixOps

private[migration] final case class GitRemotesRemovedMigrationStage(stage:GitRemotesRemovedStage, soFar:Op[MigrationResult,Nothing,MigrationStyle])
extends MigrationStage[GitRemotesRemovedStage,GitRemoteRemovedSpec] {
  override protected def changeSkippingPolicy:ChangeSkippingPolicy[GitRemoteRemovedSpec] = new SkipMaybe[GitRemoteRemovedSpec] {
    def shouldSkip(change:GitRemoteRemovedSpec):Boolean = stage willRepositoryBeRemoved (change projectID)
    def skipMessageSingular(skipped:GitRemoteRemovedSpec, state:MigrationResult):String =
      s"Skipped deleting Git remote ${skipped removedRemoteName} of project ${skipped projectID} at ${state absoluteProjectPath (skipped projectID)} because the Git repository is scheduled for removal."
    def skipMessagePlural(skipped:Traversable[GitRemoteRemovedSpec], state:MigrationResult):String =
      s"Skipped deleting Git remotes because of their repositories is scheduled for removal: ${skipped.map(ch⇒s"${ch removedRemoteName} of ${ch projectID}").toSeq.sorted mkString ", "}"
  }

  protected def changes:Traversable[GitRemoteRemovedSpec] = stage.gitRemotesRemoved
  protected def nextStage(thisResult:Op[MigrationResult,Nothing,MigrationStyle]):Op[MigrationResult,Nothing,MigrationStyle] =
    GitRepositoriesRemovedMigrationStage(stage nextStage, thisResult)
  protected def step(change:GitRemoteRemovedSpec):MigrationStep = RemoveGitRemoteStep(change)
}
