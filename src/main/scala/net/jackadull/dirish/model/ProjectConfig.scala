package net.jackadull.dirish.model

import java.util.UUID

import net.jackadull.dirish.path.{AbsolutePathSpec, RelativePathSpec}

import scala.language.postfixOps

sealed trait ProjectConfig {
  def addBaseDirectory(id:UUID, path:AbsolutePathSpec):Either[BaseDirectoryAddError,ProjectConfig]
  def addGitModule(projectID:UUID, firstRemote:(String,String)):Either[GitModuleAddError,ProjectConfig]
  def addGitModuleRemote(projectID:UUID, newRemote:(String,String)):Either[GitModuleAddRemoteError,ProjectConfig]
  def addProject(projectID:UUID, baseDirectoryID:UUID, localPath:RelativePathSpec):Either[ProjectAddError,ProjectConfig]
  def changeGitModuleFirstRemote(projectID:UUID, newFirstRemote:(String,String)):Either[GitModuleChangeFirstRemoteError,ProjectConfig]
  def moveBaseDirectory(id:UUID, newPath:AbsolutePathSpec):Either[BaseDirectoryMoveError,ProjectConfig]
  def moveProject(id:UUID, newBaseDirectoryID:UUID, newLocalPath:RelativePathSpec):Either[ProjectMoveError,ProjectConfig]
  def removeBaseDirectory(id:UUID):Either[BaseDirectoryRemoveError,ProjectConfig]
  def removeGitModule(projectID:UUID):Either[GitModuleRemoveError,ProjectConfig]
  def removeGitModuleRemote(projectID:UUID, remoteName:String):Either[GitModuleRemoveRemoteError,ProjectConfig]
  def removeProject(projectID:UUID):Either[ProjectRemoveError,ProjectConfig]
}
object ProjectConfig {
  val empty:ProjectConfig = ProjectConfigData()

  def changesBetween(from:ProjectConfig, to:ProjectConfig):ConfigChangeStages = (from,to) match {
    case (fromData:ProjectConfigData, toData:ProjectConfigData) ⇒ changesBetweenData(fromData, toData)
  }

  private def changesBetweenData(from:ProjectConfigData, to:ProjectConfigData):ConfigChangeStages = new ConfigChangeStages {
    def nextStage:GitModuleRemotesRemovedStage = gitModuleRemotesRemovedStage

    private def allAdditionalRemotes(data:ProjectConfigData):Set[(UUID,String,String)] =
      data.gitModuleAdditionalRemotes.toSet flatMap {e:(UUID,Set[(String,String)])⇒
        e._2 map {e2:(String,String) ⇒ (e _1, e2 _1, e2 _2)}
      }
    private lazy val fromAdditionalRemotes:Set[(UUID,String,String)] = allAdditionalRemotes(from)
    private lazy val toAdditionalRemotes:Set[(UUID,String,String)] = allAdditionalRemotes(to)

    private trait StageImpl {
      def baseDirectoriesAdded:Set[BaseDirectoryAddedSpec] =
        toBaseDirectoryAddedSpecs(addedEntries(from baseDirectories, to baseDirectories))
      def baseDirectoriesMoved:Set[BaseDirectoryMovedSpec] =
        toBaseDirectoryMovedSpecs(movedEntries(from baseDirectories, to baseDirectories))
      def baseDirectoriesRemoved:Set[BaseDirectoryRemovedSpec] =
        toBaseDirectoryRemovedSpecs(removedEntries(from baseDirectories, to baseDirectories))
      def gitModuleFirstRemotesChanged:Set[GitModuleFirstRemoteChangedSpec] =
        toGitModuleFirstRemotesChangedSpecs(movedEntries(from gitModules, to gitModules))
      def gitModuleRemotesAdded:Set[GitModuleRemoteAddedSpec] =
        toGitModuleRemoteAddedSpecs(toAdditionalRemotes -- fromAdditionalRemotes)
      def gitModuleRemotesRemoved:Set[GitModuleRemoteRemovedSpec] =
        toGitModuleRemoteRemovedSpecs(fromAdditionalRemotes -- toAdditionalRemotes)
      def gitModulesAdded:Set[GitModuleAddedSpec] =
        toGitModuleAddedSpecs(addedEntries(from gitModules, to gitModules))
      def gitModulesRemoved:Set[GitModuleRemovedSpec] =
        toGitModuleRemovedSpecs(removedEntries(from gitModules, to gitModules) keySet)
      def projectsAdded:Set[ProjectAddedSpec] =
        toProjectAddedSpecs(addedEntries(from projects, to projects))
      def projectsMoved:Set[ProjectMovedSpec] =
        toProjectMovedSpecs(movedEntries(from projects, to projects))
      def projectsRemoved:Set[ProjectRemovedSpec] =
        toProjectRemovedSpecs(removedEntries(from projects, to projects))
    }

    private abstract class StageWithNext[A<:ConfigChangeStage](next: ⇒A) extends StageImpl {def nextStage:A = next}

    private object gitModuleRemotesRemovedStage extends StageWithNext(gitModulesRemovedStage) with GitModuleRemotesRemovedStage
    private object gitModulesRemovedStage extends StageWithNext(projectsRemovedStage) with GitModulesRemovedStage
    private object projectsRemovedStage extends StageWithNext(baseDirectoriesRemovedStage) with ProjectsRemovedStage
    private object baseDirectoriesRemovedStage extends StageWithNext(gitModuleFirstRemotesChangedStage) with BaseDirectoriesRemovedStage
    private object gitModuleFirstRemotesChangedStage extends StageWithNext(projectsMovedStage) with GitModuleFirstRemotesChangedStage
    private object projectsMovedStage extends StageWithNext(baseDirectoriesMovedStage) with ProjectsMovedStage
    private object baseDirectoriesMovedStage extends StageWithNext(baseDirectoriesAddedStage) with BaseDirectoriesMovedStage
    private object baseDirectoriesAddedStage extends StageWithNext(projectsAddedStage) with BaseDirectoriesAddedStage
    private object projectsAddedStage extends StageWithNext(gitModulesAddedStage) with ProjectsAddedStage
    private object gitModulesAddedStage extends StageWithNext(gitModuleRemotesAddedStage) with GitModulesAddedStage
    private object gitModuleRemotesAddedStage extends StageImpl with GitModuleRemotesAddedStage
  }

  private def addedEntries[K,V](from:Map[K,V], to:Map[K,V]):Map[K,V] = to -- from.keySet
  private def movedEntries[K,V](from:Map[K,V], to:Map[K,V]):Map[K,(V,V)] = from collect {case (k,v) if to contains k ⇒ (k,(v,to(k)))}
  private def removedEntries[K,V](from:Map[K,V], to:Map[K,V]):Map[K,V] = from -- to.keySet

  private def toBaseDirectoryAddedSpecs(entries:Traversable[(UUID,AbsolutePathSpec)]):Set[BaseDirectoryAddedSpec] =
    entries.toSet map BaseDirectoryAddedSpec.tupled
  private def toBaseDirectoryMovedSpecs(entries:Traversable[(UUID,(AbsolutePathSpec,AbsolutePathSpec))]):Set[BaseDirectoryMovedSpec] =
    entries.toSet map {e:(UUID,(AbsolutePathSpec,AbsolutePathSpec)) ⇒ BaseDirectoryMovedSpec(e _1, e._2 _1, e._2 _2)}
  private def toBaseDirectoryRemovedSpecs(entries:Traversable[(UUID,AbsolutePathSpec)]):Set[BaseDirectoryRemovedSpec] =
    entries.toSet map BaseDirectoryRemovedSpec.tupled
  private def toGitModuleAddedSpecs(entries:Traversable[(UUID,(String,String))]):Set[GitModuleAddedSpec] =
    entries.toSet map {e:(UUID,(String,String)) ⇒ GitModuleAddedSpec(e _1, e _2)}
  private def toGitModuleFirstRemotesChangedSpecs(entries:Traversable[(UUID,((String,String),(String,String)))]):Set[GitModuleFirstRemoteChangedSpec] =
    entries.toSet map {e:(UUID,((String,String),(String,String))) ⇒ GitModuleFirstRemoteChangedSpec(e _1, e._2._2)}
  private def toGitModuleRemoteAddedSpecs(entries:Traversable[(UUID,String,String)]):Set[GitModuleRemoteAddedSpec] =
    entries.toSet map {e:(UUID,String,String) ⇒ GitModuleRemoteAddedSpec(e _1, (e _2, e _3))}
  private def toGitModuleRemoteRemovedSpecs(entries:Traversable[(UUID,String,String)]):Set[GitModuleRemoteRemovedSpec] =
    entries.toSet map {e:(UUID,String,String) ⇒ GitModuleRemoteRemovedSpec(e _1, e _2)}
  private def toGitModuleRemovedSpecs(entries:Traversable[UUID]):Set[GitModuleRemovedSpec] =
    entries.toSet map {id:UUID ⇒ GitModuleRemovedSpec(id)}
  private def toProjectAddedSpecs(entries:Traversable[(UUID,(UUID,RelativePathSpec))]):Set[ProjectAddedSpec] =
    entries.toSet map toProjectAddedSpec
  private def toProjectMovedSpecs(entries:Traversable[(UUID,((UUID,RelativePathSpec),(UUID,RelativePathSpec)))]):Set[ProjectMovedSpec] =
    entries.toSet map toProjectMovedSpec
  private def toProjectRemovedSpecs(entries:Traversable[(UUID,(UUID,RelativePathSpec))]):Set[ProjectRemovedSpec] =
    entries.toSet map toProjectRemovedSpec

  private def toProjectAddedSpec(entry:(UUID,(UUID,RelativePathSpec))):ProjectAddedSpec =
    ProjectAddedSpec(entry._1, ProjectLocationSpec(entry._2._1, entry._2._2))
  private def toProjectMovedSpec(entry:(UUID,((UUID,RelativePathSpec),(UUID,RelativePathSpec)))):ProjectMovedSpec =
    ProjectMovedSpec(entry._1, ProjectLocationSpec(entry._2._1._1, entry._2._1._2), ProjectLocationSpec(entry._2._2._1, entry._2._2._2))
  private def toProjectRemovedSpec(entry:(UUID,(UUID,RelativePathSpec))):ProjectRemovedSpec =
    ProjectRemovedSpec(entry._1, ProjectLocationSpec(entry._2._1, entry._2._2))
}

private final case class ProjectConfigData(
  baseDirectories:Map[UUID,AbsolutePathSpec] = Map(),
  gitModuleAdditionalRemotes:Map[UUID,Set[(String,String)]] = Map(),
  gitModules:Map[UUID,(String,String)] = Map(),
  projects:Map[UUID,(UUID,RelativePathSpec)] = Map()
) extends ProjectConfig {
  def addBaseDirectory(id:UUID, path:AbsolutePathSpec) =
    if(baseDirectories contains id) Left(BaseDirectoryWithSameIDAlreadyExists(id))
    else if(baseDirectories.values exists {_ == path}) Left(BaseDirectoryWithSamePathAlreadyExists(path))
    else {
      val crossingPaths = baseDirectories.view filter {case (_, existingPath) ⇒
        existingPath.startsWith(path) || path.startsWith(existingPath)
      }
      crossingPaths.headOption match {
        case Some((_, existingPath)) ⇒ Left(BaseDirectoryWithCrossingPathAlreadyExists(path, existingPath))
        case None ⇒ Right(copy(baseDirectories = baseDirectories + (id → path)))
      }
    }

  def addGitModule(projectID:UUID, firstRemote:(String,String)) =
    if(projects contains projectID)
      if(gitModules contains projectID) Left(ProjectAlreadyHasGitModule(projectID))
      else Right(copy(gitModules = gitModules + (projectID → firstRemote)))
    else Left(ProjectNotFoundForID(projectID))

  def addGitModuleRemote(projectID:UUID, newRemote:(String,String)) =
    if(projects contains projectID)
      if(gitModules contains projectID)
        if(gitModules(projectID) == newRemote) Left(GitModuleAlreadyHasFirstRemote(projectID, newRemote))
        else if(gitModuleAdditionalRemotes.get(projectID).exists(_.contains(newRemote))) Left(GitModuleAlreadyHasAdditionalRemote(projectID, newRemote))
        else if(gitModules(projectID)._1 == newRemote._1) Left(GitModuleAlreadyHasFirstRemoteNamed(projectID, newRemote _1))
        else if(gitModuleAdditionalRemotes.get(projectID).exists(_.exists(_._1 == newRemote._1))) Left(GitModuleAlreadyHasAdditionalRemoteNamed(projectID, newRemote _1))
        else Right(copy(gitModuleAdditionalRemotes = gitModuleAdditionalRemotes + (projectID → (gitModuleAdditionalRemotes.getOrElse(projectID, Set()) + newRemote))))
      else Left(GitModuleNotFoundForProjectID(projectID))
    else Left(ProjectNotFoundForID(projectID))

  def addProject(projectID:UUID, baseDirectoryID:UUID, localPath:RelativePathSpec) =
    if(projects contains projectID) Left(ProjectWithSameIDAlreadyExists(projectID))
    else if(!(baseDirectories contains baseDirectoryID)) Left(BaseDirectoryNotFoundForID(baseDirectoryID))
    else if(projects.values exists {p ⇒ (p._1 == baseDirectoryID) && (p._2 == localPath)}) Left(ProjectWithSamePathAlreadyExists(projectID, baseDirectoryID, localPath))
    else if(projects.values exists {p ⇒ (p._1 == baseDirectoryID) && ((p._2 startsWith localPath) || (localPath startsWith p._2))}) Left(ProjectWithCrossingPathAlreadyExists(projectID, baseDirectoryID, localPath))
    else Right(copy(projects = projects + (projectID → (baseDirectoryID, localPath))))

  def changeGitModuleFirstRemote(projectID:UUID, newFirstRemote:(String,String)) = gitModules get projectID match {
    case None ⇒
      if(!(projects contains projectID)) Left(ProjectNotFoundForID(projectID))
      else Left(GitModuleNotFoundForProjectID(projectID))
    case Some(`newFirstRemote`) ⇒ Left(GitModuleAlreadyHasFirstRemote(projectID, newFirstRemote))
    case Some(_) ⇒ gitModuleAdditionalRemotes getOrElse (projectID, Set()) match {
      case withNewFirstRemote if withNewFirstRemote contains newFirstRemote ⇒
        Left(GitModuleAlreadyHasAdditionalRemote(projectID, newFirstRemote))
      case withNewFirstRemoteName if withNewFirstRemoteName.exists(_._1 == newFirstRemote._1) ⇒
        Left(GitModuleAlreadyHasAdditionalRemoteNamed(projectID, newFirstRemote._1))
      case _ ⇒ Right(copy(gitModules = gitModules + (projectID → newFirstRemote)))
    }
  }

  def moveBaseDirectory(id:UUID, newPath:AbsolutePathSpec) = baseDirectories get id match {
    case None ⇒ Left(BaseDirectoryNotFoundForID(id))
    case Some(`newPath`) ⇒ Left(BaseDirectoryAlreadyHasPath(id, newPath))
    case Some(_) ⇒
      val crossingPaths = baseDirectories.view filter {case (existingID, existingPath) ⇒
        existingID!=id && (existingPath.startsWith(newPath) || newPath.startsWith(existingPath))
      }
      crossingPaths.headOption match {
        case Some((_, existingPath)) ⇒ Left(BaseDirectoryWithCrossingPathAlreadyExists(newPath, existingPath))
        case None ⇒ Right(copy(baseDirectories = baseDirectories + (id → newPath)))
      }
  }

  // TODO what if I move a project into a sub-folder of that project?
  // TODO what if I move a project into a parent folder of that project?
  def moveProject(id:UUID, newBaseDirectoryID:UUID, newLocalPath:RelativePathSpec) =
    if(!(baseDirectories contains newBaseDirectoryID)) Left(BaseDirectoryNotFoundForID(newBaseDirectoryID))
    else if(!(projects contains id)) Left(ProjectNotFoundForID(id))
    else if(projects(id) == (newBaseDirectoryID, newLocalPath)) Left(ProjectAlreadyHasPath(id, newBaseDirectoryID, newLocalPath))
    else if(projects.values exists {_ == (newBaseDirectoryID, newLocalPath)}) Left(ProjectWithSamePathAlreadyExists(id, newBaseDirectoryID, newLocalPath))
    else if(projects exists {case (pid:UUID,(`newBaseDirectoryID`,lp:RelativePathSpec)) ⇒ (pid != id) && ((lp startsWith newLocalPath) || (newLocalPath startsWith lp))}) Left(ProjectWithCrossingPathAlreadyExists(id, newBaseDirectoryID, newLocalPath))
    else Right(copy(projects = projects + (id → (newBaseDirectoryID, newLocalPath))))

  def removeBaseDirectory(id:UUID) =
    if(baseDirectories contains id)
      if(projects contains id) Left(BaseDirectoryStillReferencedByProject(id))
      else Right(copy(baseDirectories = baseDirectories - id))
    else Left(BaseDirectoryNotFoundForID(id))

  def removeGitModule(projectID:UUID) =
    if(projects contains projectID)
      if(gitModules contains projectID)
        if(gitModuleAdditionalRemotes contains projectID) Left(GitModuleStillHasAdditionalRemotes(projectID))
        else Right(copy(gitModules = gitModules - projectID))
      else Left(GitModuleNotFoundForProjectID(projectID))
    else Left(ProjectNotFoundForID(projectID))

  def removeGitModuleRemote(projectID:UUID, remoteName:String) =
    if(projects contains projectID)
      if(gitModules contains projectID)
        if(gitModules(projectID)._1 == remoteName) Left(CannotRemoveGitModuleFirstRemote(projectID, remoteName))
        else gitModuleAdditionalRemotes get projectID match {
          case None ⇒ Left(GitModuleRemoteNotFoundForName(projectID, remoteName))
          case Some(withoutRemoteName) if !(withoutRemoteName exists (_._1==remoteName)) ⇒ Left(GitModuleRemoteNotFoundForName(projectID, remoteName))
          case Some(withRemoteName) ⇒
            if(withRemoteName.size == 1) Right(copy(gitModuleAdditionalRemotes = gitModuleAdditionalRemotes - projectID))
            else Right(copy(gitModuleAdditionalRemotes = gitModuleAdditionalRemotes + (projectID → gitModuleAdditionalRemotes(projectID).filterNot(_._1 == remoteName))))
        }
      else Left(GitModuleNotFoundForProjectID(projectID))
    else Left(ProjectNotFoundForID(projectID))

  def removeProject(projectID:UUID) =
    if(!(projects contains projectID)) Left(ProjectNotFoundForID(projectID))
    else if(gitModules contains projectID) Left(ProjectStillReferencedByGitModule(projectID))
    else Right(copy(projects = projects - projectID))
}
