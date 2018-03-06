package net.jackadull.dirish.migration.stage

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationResult
import net.jackadull.dirish.migration.step.{ChangeGitFirstRemoteStep, MigrationStep}
import net.jackadull.dirish.model.{GitFirstRemoteChangedSpec, GitFirstRemotesChangedStage}
import net.jackadull.dirish.op.Op

import scala.language.postfixOps

private[stage] final case class GitFirstRemotesChangedMigrationStage(stage:GitFirstRemotesChangedStage, soFar:Op[MigrationResult,Nothing,MigrationStyle])
extends MigrationStage[GitFirstRemotesChangedStage,GitFirstRemoteChangedSpec] {
  protected def changes:Traversable[GitFirstRemoteChangedSpec] = stage.gitFirstRemotesChanged
  protected def nextStage(thisResult:Op[MigrationResult,Nothing,MigrationStyle]):Op[MigrationResult,Nothing,MigrationStyle] =
    ProjectsMovedMigrationStage(stage nextStage, thisResult)
  protected def step(change:GitFirstRemoteChangedSpec):MigrationStep = ChangeGitFirstRemoteStep(change)
}
