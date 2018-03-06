package net.jackadull.dirish.migration.stage

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationResult
import net.jackadull.dirish.migration.step.{AddGitRepositoryStep, MigrationStep}
import net.jackadull.dirish.model.{GitRepositoriesAddedStage, GitRepositoryAddedSpec}
import net.jackadull.dirish.op.Op

import scala.language.postfixOps

private[stage] final case class GitRepositoriesAddedMigrationStage(stage:GitRepositoriesAddedStage, soFar:Op[MigrationResult,Nothing,MigrationStyle])
extends MigrationStage[GitRepositoriesAddedStage,GitRepositoryAddedSpec] {
  protected def changes:Traversable[GitRepositoryAddedSpec] = stage.gitRepositoriesAdded
  protected def nextStage(thisResult:Op[MigrationResult,Nothing,MigrationStyle]):Op[MigrationResult,Nothing,MigrationStyle] =
    GitRemotesAddedMigrationStage(stage nextStage, thisResult)
  protected def step(change:GitRepositoryAddedSpec):MigrationStep = AddGitRepositoryStep(change)
}
