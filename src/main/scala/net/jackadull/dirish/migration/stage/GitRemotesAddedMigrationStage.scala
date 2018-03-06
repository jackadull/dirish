package net.jackadull.dirish.migration.stage

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationResult
import net.jackadull.dirish.migration.step.{AddGitRemoteStep, MigrationStep}
import net.jackadull.dirish.model.{GitRemoteAddedSpec, GitRemotesAddedStage}
import net.jackadull.dirish.op.Op

private[stage] final case class GitRemotesAddedMigrationStage(stage:GitRemotesAddedStage, soFar:Op[MigrationResult,Nothing,MigrationStyle])
extends MigrationStage[GitRemotesAddedStage,GitRemoteAddedSpec] {
  protected def changes:Traversable[GitRemoteAddedSpec] = stage.gitRemotesAdded
  protected def nextStage(thisResult:Op[MigrationResult,Nothing,MigrationStyle]):Op[MigrationResult,Nothing,MigrationStyle] =
    thisResult
  protected def step(change:GitRemoteAddedSpec):MigrationStep = AddGitRemoteStep(change)
}
