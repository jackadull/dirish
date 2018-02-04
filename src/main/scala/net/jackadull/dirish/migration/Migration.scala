package net.jackadull.dirish.migration

import java.util.UUID

import net.jackadull.dirish.io.IODSL._
import net.jackadull.dirish.io.LogCategory.SkippedChangeForDownstreamChange
import net.jackadull.dirish.io._
import net.jackadull.dirish.model._
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.collection.immutable.ListSet
import scala.language.postfixOps

// TODO maybe save the current state between stages, or even between steps
object Migration {
  final case class MigrationResult(state:ProjectConfig, changesNotPerformed:Set[ConfigChangeSpec]=Set(), changesSkippedForDownstream:ListSet[ConfigChangeSpec]=ListSet()) extends CustomIOResult {
    def absoluteProjectPath(projectID:UUID):AbsolutePathSpec = baseDirectoryPath(state.projectBaseDirectoryID(projectID).get)/state.projectLocalPath(projectID).get
    def baseDirectoryPath(baseDirectoryID:UUID):AbsolutePathSpec = state.baseDirectoryPath(baseDirectoryID).get
    def doesProjectHaveGitModule(projectID:UUID):Boolean = state.projectFirstRemote(projectID).isDefined
    def firstRemoteNameOfProject(projectID:UUID):String = state.projectFirstRemote(projectID).get._1
    def projectIDsWithBaseDirectory(baseDirectoryID:UUID):Set[UUID] = state.projectIDs filter {pid ⇒ state.projectBaseDirectoryID(pid) contains baseDirectoryID}

    def failedChange(change:ConfigChangeSpec):MigrationResult = copy(changesNotPerformed = changesNotPerformed + change)
    def notPerformingChangeTransitive(changeNotPerformed:ConfigChangeSpec):MigrationResult = copy(changesNotPerformed = changesNotPerformed + changeNotPerformed)
    def performedChange(change:ConfigChangeSpec):MigrationResult = copy(state = change.applyTo(state).right.get)
    def performedImplicitlySkippedUpstreamChange(skipped:ConfigChangeSpec):MigrationResult = copy(changesNotPerformed = changesNotPerformed - skipped, changesSkippedForDownstream = changesSkippedForDownstream - skipped).performedChange(skipped)
    def skippedBecauseOfDownstreamChange(change:ConfigChangeSpec):MigrationResult = copy(changesNotPerformed = changesNotPerformed + change, changesSkippedForDownstream = changesSkippedForDownstream + change)
  }

  def apply(stages:ConfigChangeStages, state:ProjectConfig):IOOp[MigrationResult] =
    gitModuleRemotesRemovedStage(stages nextStage, IOBind(MigrationResult(state)))

  private def gitModuleRemotesRemovedStage(stage:GitModuleRemotesRemovedStage, soFar:IOOp[MigrationResult]):IOOp[MigrationResult] = {
    // TODO separate common code out from this one and the next two
    val allToRemove = stage.gitModuleRemotesRemoved
    val (withRemovedModulesScheduledForRemoval, withRetainedModules) =
      allToRemove.partition(toRemove ⇒ stage willModuleBeRemoved toRemove.projectID)
    val soFar2 = soFar flatMap {state ⇒
      val state2 = withRemovedModulesScheduledForRemoval.foldLeft(state) {_ skippedBecauseOfDownstreamChange _}
      withRemovedModulesScheduledForRemoval.size match {
        case 0 ⇒ IOBind(state2)
        case 1 ⇒
          val change = withRemovedModulesScheduledForRemoval.head
          Log(SkippedChangeForDownstreamChange, s"Skipped deleting Git remote ${change removedRemoteName} of project ${change projectID} at ${state2 absoluteProjectPath (change projectID)} because the Git module is scheduled for removal.") map {_ ⇒ state2}
        case _ ⇒
          Log(SkippedChangeForDownstreamChange, s"Skipped deleting Git remotes because of their module is scheduled for removal: ${withRemovedModulesScheduledForRemoval.map(ch⇒s"${ch removedRemoteName} of ${ch projectID}").toSeq.sorted mkString ", "}") map {_ ⇒ state2}
      }
    }
    val local = MigrationStep.applyInSequence(withRetainedModules map RemoveGitModuleRemoteStep, soFar2)
    gitModulesRemovedStage(stage nextStage, local)
  }

  private def gitModulesRemovedStage(stage:GitModulesRemovedStage, soFar:IOOp[MigrationResult]):IOOp[MigrationResult] = {
    val allToRemove = stage.gitModulesRemoved
    val (withRemovedProjectsScheduledForRemoval,withRetainedProjects) =
      allToRemove.partition(toRemove ⇒ stage willProjectBeRemoved toRemove.projectID)
    val soFar2 = soFar flatMap {state ⇒
      val state2 = withRemovedProjectsScheduledForRemoval.foldLeft(state) {_ skippedBecauseOfDownstreamChange _}
      withRemovedProjectsScheduledForRemoval.size match {
        case 0 ⇒ IOBind(state2)
        case 1 ⇒
          val change = withRemovedProjectsScheduledForRemoval.head
          Log(SkippedChangeForDownstreamChange, s"Skipped deleting Git module of project ${change projectID} at ${state2 absoluteProjectPath (change projectID)} because the project is scheduled for removal.") map {_ ⇒ state2}
        case _ ⇒
          Log(SkippedChangeForDownstreamChange, s"Skipped deleting Git modules because their projects are scheduled for removal: ${withRemovedProjectsScheduledForRemoval.map(_.projectID.toString).toSeq.sorted mkString ", "}") map {_ ⇒ state2}
      }
    }
    val local = MigrationStep.applyInSequence(withRetainedProjects map RemoveGitModuleStep, soFar2)
    projectsRemovedStage(stage nextStage, local)
  }

  private def projectsRemovedStage(stage:ProjectsRemovedStage, soFar:IOOp[MigrationResult]):IOOp[MigrationResult] = {
    val allToRemove = stage.projectsRemoved
    val (withRemovedBaseDirsScheduledForRemoval,withRetainedBaseDirs) =
      allToRemove.partition(toRemove ⇒ stage willBaseDirectoryBeRemoved (toRemove.location baseDirectoryID))
    val soFar2 = soFar flatMap {state ⇒
      val state2 = withRemovedBaseDirsScheduledForRemoval.foldLeft(state) {_ skippedBecauseOfDownstreamChange _}
      withRemovedBaseDirsScheduledForRemoval.size match {
        case 0 ⇒ IOBind(state2)
        case 1 ⇒
          val change = withRemovedBaseDirsScheduledForRemoval.head
          Log(SkippedChangeForDownstreamChange, s"Skipped deleting project ${change id} at ${state2 absoluteProjectPath (change id)} because its base directory is scheduled for removal.") map {_ ⇒ state2}
        case _ ⇒
          Log(SkippedChangeForDownstreamChange, s"Skipped deleting projects because their base directory is scheduled for removal: ${withRemovedBaseDirsScheduledForRemoval.map(_.id.toString).toSeq.sorted mkString ", "}") map {_ ⇒ state2}
      }
    }
    val local = MigrationStep.applyInSequence(withRetainedBaseDirs map RemoveProjectStep, soFar2)
    baseDirectoriesRemovedStage(stage nextStage, local)
  }

  private def baseDirectoriesRemovedStage(stage:BaseDirectoriesRemovedStage, soFar:IOOp[MigrationResult]):IOOp[MigrationResult] = {
    val local = MigrationStep.applyInSequence(stage.baseDirectoriesRemoved map RemoveBaseDirectoryStep, soFar)
    gitModuleFirstRemotesChangedStage(stage nextStage, local)
  }

  private def gitModuleFirstRemotesChangedStage(stage:GitModuleFirstRemotesChangedStage, soFar:IOOp[MigrationResult]):IOOp[MigrationResult] = {
    val local = MigrationStep.applyInSequence(stage.gitModuleFirstRemotesChanged map ChangeGitModuleFirstRemoteStep, soFar)
    projectsMovedStage(stage nextStage, local)
  }

  private def projectsMovedStage(stage:ProjectsMovedStage, soFar:IOOp[MigrationResult]):IOOp[MigrationResult] = {
    // TODO detect loops and dependencies
    val local = MigrationStep.applyInSequence(stage.projectsMoved map MoveProjectStep, soFar)
    baseDirectoriesMovedStage(stage nextStage, local)
  }

  private def baseDirectoriesMovedStage(stage:BaseDirectoriesMovedStage, soFar:IOOp[MigrationResult]):IOOp[MigrationResult] = {
    // TODO detect loops and dependencies
    val local = MigrationStep.applyInSequence(stage.baseDirectoriesMoved map MoveBaseDirectoryStep, soFar)
    baseDirectoriesAddedStage(stage nextStage, local)
  }

  private def baseDirectoriesAddedStage(stage:BaseDirectoriesAddedStage, soFar:IOOp[MigrationResult]):IOOp[MigrationResult] = {
    val local = MigrationStep.applyInSequence(stage.baseDirectoriesAdded map AddBaseDirectoryStep, soFar)
    projectsAddedStage(stage nextStage, local)
  }

  private def projectsAddedStage(stage:ProjectsAddedStage, soFar:IOOp[MigrationResult]):IOOp[MigrationResult] = {
    val local = MigrationStep.applyInSequence(stage.projectsAdded map AddProjectStep, soFar)
    gitModulesAddedStage(stage nextStage, local)
  }

  private def gitModulesAddedStage(stage:GitModulesAddedStage, soFar:IOOp[MigrationResult]):IOOp[MigrationResult] = {
    val local = MigrationStep.applyInParallel(stage.gitModulesAdded map AddGitModuleStep, soFar)
    gitModuleRemotesAddedStage(stage nextStage, local)
  }

  private def gitModuleRemotesAddedStage(stage:GitModuleRemotesAddedStage, soFar:IOOp[MigrationResult]):IOOp[MigrationResult] =
    MigrationStep.applyInSequence(stage.gitModuleRemotesAdded map AddGitModuleRemoteStep, soFar)

  // TODO refactor into separate table
  // TODO if project shall be added, but base directory from the same location could not be moved or removed -> can't add project
  def shouldNotPerformTransitively(changeInQuestion:ConfigChangeSpec, changesNotPerfomed:Traversable[ConfigChangeSpec]):Boolean =
    changesNotPerfomed exists {notPerformed ⇒ (changeInQuestion, notPerformed) match {
      case (a:BaseDirectoryAddedSpec, b:BaseDirectoryMovedSpec) if a.path == b.from ⇒ true
      case (a:BaseDirectoryAddedSpec, b:BaseDirectoryRemovedSpec) if a.path == b.path ⇒ true
      case (a:BaseDirectoryMovedSpec, b:BaseDirectoryMovedSpec) if a.to == b.from ⇒ true
      case (a:BaseDirectoryMovedSpec, b:BaseDirectoryRemovedSpec) if a.to == b.path ⇒ true
      case (a:GitModuleAddedSpec, b:ProjectAddedSpec) if a.projectID == b.id ⇒ true
      case (a:GitModuleFirstRemoteChangedSpec, b:GitModuleRemoteRemovedSpec) if a.projectID == b.projectID ⇒ true
      case (a:GitModuleRemoteAddedSpec, b:GitModuleAddedSpec) if a.projectID == b.projectID ⇒ true
      case (a:GitModuleRemoteAddedSpec, b:GitModuleFirstRemoteChangedSpec) if a.projectID == b.projectID ⇒ true
      case (a:GitModuleRemoteAddedSpec, b:GitModuleRemoteAddedSpec) if a.projectID == b.projectID ⇒ true
      case (a:GitModuleRemoteAddedSpec, b:GitModuleRemoteRemovedSpec) if a.projectID == b.projectID ⇒ true
      case (a:ProjectAddedSpec, b:BaseDirectoryAddedSpec) if a.location.baseDirectoryID == b.id ⇒ true
      case (a:ProjectAddedSpec, b:ProjectMovedSpec) if a.location == b.from ⇒ true
      case (a:ProjectAddedSpec, b:ProjectRemovedSpec) if a.location == b.location ⇒ true
      case (a:ProjectMovedSpec, b:BaseDirectoryAddedSpec) if a.to.baseDirectoryID == b.id ⇒ true
      case (a:ProjectMovedSpec, b:ProjectMovedSpec) if a.to == b.from ⇒ true
      case (a:ProjectMovedSpec, b:ProjectRemovedSpec) if a.to == b.location ⇒ true
      case _ ⇒ false
    }}
}
