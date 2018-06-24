package net.jackadull.dirish.model

import java.util.UUID

trait ConfigChangeStages extends ConfigChangeStageWithNextStage {
  override def nextStage:GitRemotesRemovedStage
}

sealed trait ConfigChangeStage
sealed trait ConfigChangeStageWithNextStage extends ConfigChangeStage {
  def nextStage:ConfigChangeStage
}

trait GitRemotesRemovedStage extends ConfigChangeStageWithNextStage {
  def gitRemotesRemoved:Set[GitRemoteRemovedSpec]
  override def nextStage:GitRepositoriesRemovedStage
  def willRepositoryBeRemoved(projectID:UUID):Boolean =
    nextStage.gitRepositoriesRemoved.exists(_.projectID==projectID)
}
trait GitRepositoriesRemovedStage extends ConfigChangeStageWithNextStage {
  def gitRepositoriesRemoved:Set[GitRepositoryRemovedSpec]
  override def nextStage:ProjectActiveSignalsRemovedStage
  def willProjectBeRemoved(projectID:UUID):Boolean =
    nextStage.willProjectBeRemoved(projectID)
}
trait ProjectActiveSignalsRemovedStage extends ConfigChangeStageWithNextStage {
  def projectActiveSignalsRemoved:Set[ProjectActiveSignalRemovedSpec]
  override def nextStage:ProjectsRemovedStage
  def willProjectBeRemoved(projectID:UUID):Boolean =
    nextStage.projectsRemoved.exists(_.id==projectID)
}
trait ProjectsRemovedStage extends ConfigChangeStageWithNextStage {
  def projectsRemoved:Set[ProjectRemovedSpec]
  override def nextStage:BaseDirectoriesRemovedStage
  def willBaseDirectoryBeRemoved(baseDirectoryID:UUID):Boolean =
    nextStage.baseDirectoriesRemoved.exists(_.id==baseDirectoryID)
}
trait BaseDirectoriesRemovedStage extends ConfigChangeStageWithNextStage {
  def baseDirectoriesRemoved:Set[BaseDirectoryRemovedSpec]
  override def nextStage:GitFirstRemotesChangedStage
}
trait GitFirstRemotesChangedStage extends ConfigChangeStageWithNextStage {
  def gitFirstRemotesChanged:Set[GitFirstRemoteChangedSpec]
  override def nextStage:ProjectsMovedStage
}
trait ProjectsMovedStage extends ConfigChangeStageWithNextStage {
  def projectsMoved:Set[ProjectMovedSpec]
  override def nextStage:BaseDirectoriesMovedStage
}
trait BaseDirectoriesMovedStage extends ConfigChangeStageWithNextStage {
  def baseDirectoriesMoved:Set[BaseDirectoryMovedSpec]
  override def nextStage:BaseDirectoriesAddedStage
}
trait BaseDirectoriesAddedStage extends ConfigChangeStageWithNextStage {
  def baseDirectoriesAdded:Set[BaseDirectoryAddedSpec]
  override def nextStage:ProjectsAddedStage
}
trait ProjectsAddedStage extends ConfigChangeStageWithNextStage {
  def projectsAdded:Set[ProjectAddedSpec]
  override def nextStage:ProjectActiveSignalsAddedStage
}
trait ProjectActiveSignalsAddedStage extends ConfigChangeStageWithNextStage {
  def projectActiveSignalsAdded:Set[ProjectActiveSignalAddedSpec]
  override def nextStage:GitRepositoriesAddedStage
}
trait GitRepositoriesAddedStage extends ConfigChangeStageWithNextStage {
  def gitRepositoriesAdded:Set[GitRepositoryAddedSpec]
  override def nextStage:GitRemotesAddedStage
}
trait GitRemotesAddedStage extends ConfigChangeStage {
  def gitRemotesAdded:Set[GitRemoteAddedSpec]
}
