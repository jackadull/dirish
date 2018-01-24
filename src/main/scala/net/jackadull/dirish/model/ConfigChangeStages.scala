package net.jackadull.dirish.model

trait ConfigChangeStages extends ConfigChangeStageWithNextStage {
  override def nextStage:GitModuleRemotesRemovedStage
}

// TODO add helpers for detecting dependencies and/or circularities

sealed trait ConfigChangeStage
sealed trait ConfigChangeStageWithNextStage extends ConfigChangeStage {
  def nextStage:ConfigChangeStage
}

trait GitModuleRemotesRemovedStage extends ConfigChangeStageWithNextStage {
  def gitModuleRemotesRemoved:Set[GitModuleRemoteRemovedSpec]
  override def nextStage:GitModulesRemovedStage
}
trait GitModulesRemovedStage extends ConfigChangeStageWithNextStage {
  def gitModulesRemoved:Set[GitModuleRemovedSpec]
  override def nextStage:ProjectsRemovedStage
}
trait ProjectsRemovedStage extends ConfigChangeStageWithNextStage {
  def projectsRemoved:Set[ProjectRemovedSpec]
  override def nextStage:BaseDirectoriesRemovedStage
}
trait BaseDirectoriesRemovedStage extends ConfigChangeStageWithNextStage {
  def baseDirectoriesRemoved:Set[BaseDirectoryRemovedSpec]
  override def nextStage:GitModuleFirstRemotesChangedStage
}
trait GitModuleFirstRemotesChangedStage extends ConfigChangeStageWithNextStage {
  def gitModuleFirstRemotesChanged:Set[GitModuleFirstRemoteChangedSpec]
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
  override def nextStage:GitModulesAddedStage
}
trait GitModulesAddedStage extends ConfigChangeStageWithNextStage {
  def gitModulesAdded:Set[GitModuleAddedSpec]
  override def nextStage:GitModuleRemotesAddedStage
}
trait GitModuleRemotesAddedStage extends ConfigChangeStage {
  def gitModuleRemotesAdded:Set[GitModuleRemoteAddedSpec]
}
