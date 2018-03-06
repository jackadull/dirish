package net.jackadull.dirish.migration

import java.util.UUID

import net.jackadull.dirish.model._
import net.jackadull.dirish.op.OpError
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.collection.immutable.ListSet
import scala.language.postfixOps

final case class MigrationResult(
  state:ProjectConfig,
  errorOpt:Option[OpError]=None,
  lastSavedState:Option[ProjectConfig]=None,
  changesNotPerformed:Set[ConfigChangeSpec]=Set(),
  changesSkippedForDownstream:ListSet[ConfigChangeSpec]=ListSet(),
  changesSkippedBecauseNotAllSignalsAreUp:ListSet[ConfigChangeSpec]=ListSet(),
  failures:Set[(ConfigChangeSpec,OpError)]=Set()
) {
  def absoluteProjectPath(projectID:UUID):AbsolutePathSpec = baseDirectoryPath(state.projectBaseDirectoryID(projectID).get)/state.projectLocalPath(projectID).get
  def baseDirectoryPath(baseDirectoryID:UUID):AbsolutePathSpec = state.baseDirectoryPath(baseDirectoryID).get
  def doesProjectHaveGitRepository(projectID:UUID):Boolean = state.projectFirstRemote(projectID).isDefined
  def firstRemoteNameOfProject(projectID:UUID):String = state.projectFirstRemote(projectID).get._1
  def projectIDsWithBaseDirectory(baseDirectoryID:UUID):Set[UUID] = state.projectIDs filter {pid ⇒ state.projectBaseDirectoryID(pid) contains baseDirectoryID}

  def failedChange(change:ConfigChangeSpec, error:OpError):MigrationResult = copy(changesNotPerformed = changesNotPerformed + change, failures = failures + (change → error))
  def notPerformingChangeTransitive(changeNotPerformed:ConfigChangeSpec):MigrationResult = copy(changesNotPerformed = changesNotPerformed + changeNotPerformed)
  def performedChange(change:ConfigChangeSpec):MigrationResult = copy(state = getRight(change.applyTo(state)))
  def performedImplicitlySkippedUpstreamChange(skipped:ConfigChangeSpec):MigrationResult = copy(changesNotPerformed = changesNotPerformed - skipped, changesSkippedForDownstream = changesSkippedForDownstream - skipped).performedChange(skipped)
  def skippedBecauseNotAllSignalsAreUp(change:ConfigChangeSpec):MigrationResult = copy(changesNotPerformed = changesNotPerformed + change)
  def skippedBecauseOfDownstreamChange(change:ConfigChangeSpec):MigrationResult = copy(changesNotPerformed = changesNotPerformed + change, changesSkippedForDownstream = changesSkippedForDownstream + change, changesSkippedBecauseNotAllSignalsAreUp = changesSkippedBecauseNotAllSignalsAreUp + change)

  private def getRight[A,B](either:Either[A,B]):B = either match {
    case Left(left) ⇒ sys error s"expected Right, but got Left($left)"
    case Right(right) ⇒ right
  }
}
