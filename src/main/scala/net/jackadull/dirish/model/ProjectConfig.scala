package net.jackadull.dirish.model

import java.util.UUID

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.op.OpError
import net.jackadull.dirish.op.signals.Signal
import net.jackadull.dirish.path.{AbsolutePathSpec, RelativePathSpec}

import scala.language.postfixOps

sealed trait ProjectConfig {
  def activeSignals(projectID:UUID):Set[Signal[Boolean,OpError,MigrationStyle]]
  def baseDirectoryIDs:Set[UUID]
  def baseDirectoryPath(baseDirectoryID:UUID):Option[AbsolutePathSpec]
  def projectAdditionalGitRemotes(projectID:UUID):Set[(String,String)]
  def projectBaseDirectoryID(projectID:UUID):Option[UUID]
  def projectFirstRemote(projectID:UUID):Option[(String,String)]
  def projectIDs:Set[UUID]
  def projectLocalPath(projectID:UUID):Option[RelativePathSpec]

  def addBaseDirectory(id:UUID, path:AbsolutePathSpec):Either[BaseDirectoryAddError,ProjectConfig]
  def addGitRepository(projectID:UUID, firstRemote:(String,String)):Either[GitRepositoryAddError,ProjectConfig]
  def addGitRepositoryRemote(projectID:UUID, newRemote:(String,String)):Either[GitAddRemoteError,ProjectConfig]
  def addProject(projectID:UUID, baseDirectoryID:UUID, localPath:RelativePathSpec):Either[ProjectAddError,ProjectConfig]
  def addProjectActiveSignal(projectID:UUID, signal:Signal[Boolean,OpError,MigrationStyle]):Either[ProjectActiveSignalAddError,ProjectConfig]
  def changeGitFirstRemote(projectID:UUID, newFirstRemote:(String,String)):Either[GitChangeFirstRemoteError,ProjectConfig]
  def moveBaseDirectory(id:UUID, newPath:AbsolutePathSpec):Either[BaseDirectoryMoveError,ProjectConfig]
  def moveProject(id:UUID, newBaseDirectoryID:UUID, newLocalPath:RelativePathSpec):Either[ProjectMoveError,ProjectConfig]
  def removeBaseDirectory(id:UUID):Either[BaseDirectoryRemoveError,ProjectConfig]
  def removeGitRepository(projectID:UUID):Either[GitRepositoryRemoveError,ProjectConfig]
  def removeGitRemote(projectID:UUID, remoteName:String):Either[GitRemoveRemoteError,ProjectConfig]
  def removeProject(projectID:UUID):Either[ProjectRemoveError,ProjectConfig]
  def removeProjectActiveSignal(projectID:UUID, signal:Signal[Boolean,OpError,MigrationStyle]):Either[ProjectActiveSignalRemoveError,ProjectConfig]
}
object ProjectConfig {
  val empty:ProjectConfig = ProjectConfigData()

  def changesBetween(from:ProjectConfig, to:ProjectConfig):ConfigChangeStages = (from,to) match {
    case (fromData:ProjectConfigData, toData:ProjectConfigData) ⇒ changesBetweenData(fromData, toData)
  }

  private def changesBetweenData(from:ProjectConfigData, to:ProjectConfigData):ConfigChangeStages = new ConfigChangeStages {
    def nextStage:GitRemotesRemovedStage = gitRemotesRemovedStage

    private def allAdditionalRemotes(data:ProjectConfigData):Set[(UUID,String,String)] =
      data.gitAdditionalRemotes.toSet flatMap {e:(UUID,Set[(String,String)])⇒
        e._2 map {e2:(String,String) ⇒ (e _1, e2 _1, e2 _2)}
      }
    private def allProjectActiveSignals(data:ProjectConfigData):Set[(UUID,Signal[Boolean,OpError,MigrationStyle])] =
      data.projectActiveSignals.toSet flatMap {e:(UUID,Set[Signal[Boolean,OpError,MigrationStyle]])⇒
        e._2 map {e2:Signal[Boolean,OpError,MigrationStyle] ⇒ (e _1, e2)}
      }
    private lazy val fromAdditionalRemotes:Set[(UUID,String,String)] = allAdditionalRemotes(from)
    private lazy val fromProjectActiveSignals:Set[(UUID,Signal[Boolean,OpError,MigrationStyle])] = allProjectActiveSignals(from)
    private lazy val toAdditionalRemotes:Set[(UUID,String,String)] = allAdditionalRemotes(to)
    private lazy val toProjectActiveSignals:Set[(UUID,Signal[Boolean,OpError,MigrationStyle])] = allProjectActiveSignals(to)

    private trait StageImpl {
      lazy val baseDirectoriesAdded:Set[BaseDirectoryAddedSpec] =
        toBaseDirectoryAddedSpecs(addedEntries(from baseDirectories, to baseDirectories))
      lazy val baseDirectoriesMoved:Set[BaseDirectoryMovedSpec] =
        toBaseDirectoryMovedSpecs(movedEntries(from baseDirectories, to baseDirectories))
      lazy val baseDirectoriesRemoved:Set[BaseDirectoryRemovedSpec] =
        toBaseDirectoryRemovedSpecs(removedEntries(from baseDirectories, to baseDirectories))
      lazy val gitFirstRemotesChanged:Set[GitFirstRemoteChangedSpec] =
        toGitFirstRemotesChangedSpecs(movedEntries(from gitRepositories, to gitRepositories))
      lazy val gitRemotesAdded:Set[GitRemoteAddedSpec] =
        toGitRemoteAddedSpecs(toAdditionalRemotes -- fromAdditionalRemotes)
      lazy val gitRemotesRemoved:Set[GitRemoteRemovedSpec] =
        toGitRemoteRemovedSpecs(fromAdditionalRemotes -- toAdditionalRemotes)
      lazy val gitRepositoriesAdded:Set[GitRepositoryAddedSpec] =
        toGitRepositoryAddedSpecs(addedEntries(from gitRepositories, to gitRepositories))
      lazy val gitRepositoriesRemoved:Set[GitRepositoryRemovedSpec] =
        toGitRepositoryRemovedSpecs(removedEntries(from gitRepositories, to gitRepositories) keySet)
      lazy val projectActiveSignalsAdded:Set[ProjectActiveSignalAddedSpec] =
        toProjectActiveSignalsAddedSpecs(toProjectActiveSignals -- fromProjectActiveSignals)
      lazy val projectActiveSignalsRemoved:Set[ProjectActiveSignalRemovedSpec] =
        toProjectActiveSignalsRemovedSpecs(fromProjectActiveSignals -- toProjectActiveSignals)
      lazy val projectsAdded:Set[ProjectAddedSpec] =
        toProjectAddedSpecs(addedEntries(from projects, to projects))
      lazy val projectsMoved:Set[ProjectMovedSpec] =
        toProjectMovedSpecs(movedEntries(from projects, to projects))
      lazy val projectsRemoved:Set[ProjectRemovedSpec] =
        toProjectRemovedSpecs(removedEntries(from projects, to projects))
    }

    private abstract class StageWithNext[A<:ConfigChangeStage](next: ⇒A) extends StageImpl {def nextStage:A = next}

    private object gitRemotesRemovedStage extends StageWithNext(gitRepositoriesRemovedStage) with GitRemotesRemovedStage
    private object gitRepositoriesRemovedStage extends StageWithNext(projectActiveSignalsRemovedStage) with GitRepositoriesRemovedStage
    private object projectActiveSignalsRemovedStage extends StageWithNext(projectsRemovedStage) with ProjectActiveSignalsRemovedStage
    private object projectsRemovedStage extends StageWithNext(baseDirectoriesRemovedStage) with ProjectsRemovedStage
    private object baseDirectoriesRemovedStage extends StageWithNext(gitFirstRemotesChangedStage) with BaseDirectoriesRemovedStage
    private object gitFirstRemotesChangedStage extends StageWithNext(projectsMovedStage) with GitFirstRemotesChangedStage
    private object projectsMovedStage extends StageWithNext(baseDirectoriesMovedStage) with ProjectsMovedStage
    private object baseDirectoriesMovedStage extends StageWithNext(baseDirectoriesAddedStage) with BaseDirectoriesMovedStage
    private object baseDirectoriesAddedStage extends StageWithNext(projectsAddedStage) with BaseDirectoriesAddedStage
    private object projectsAddedStage extends StageWithNext(projectActiveSignalsAddedStage) with ProjectsAddedStage
    private object projectActiveSignalsAddedStage extends StageWithNext(gitRepositoriesAddedStage) with ProjectActiveSignalsAddedStage
    private object gitRepositoriesAddedStage extends StageWithNext(gitRemotesAddedStage) with GitRepositoriesAddedStage
    private object gitRemotesAddedStage extends StageImpl with GitRemotesAddedStage
  }

  private def addedEntries[K,V](from:Map[K,V], to:Map[K,V]):Map[K,V] = to -- from.keySet
  private def movedEntries[K,V](from:Map[K,V], to:Map[K,V]):Map[K,(V,V)] = from collect {case (k,v) if (to contains k) && (to(k)!=v) ⇒ (k,(v,to(k)))}
  private def removedEntries[K,V](from:Map[K,V], to:Map[K,V]):Map[K,V] = from -- to.keySet

  private def toBaseDirectoryAddedSpecs(entries:Traversable[(UUID,AbsolutePathSpec)]):Set[BaseDirectoryAddedSpec] =
    entries.toSet map BaseDirectoryAddedSpec.tupled
  private def toBaseDirectoryMovedSpecs(entries:Traversable[(UUID,(AbsolutePathSpec,AbsolutePathSpec))]):Set[BaseDirectoryMovedSpec] =
    entries.toSet map {e:(UUID,(AbsolutePathSpec,AbsolutePathSpec)) ⇒ BaseDirectoryMovedSpec(e _1, e._2 _1, e._2 _2)}
  private def toBaseDirectoryRemovedSpecs(entries:Traversable[(UUID,AbsolutePathSpec)]):Set[BaseDirectoryRemovedSpec] =
    entries.toSet map BaseDirectoryRemovedSpec.tupled
  private def toGitRepositoryAddedSpecs(entries:Traversable[(UUID,(String,String))]):Set[GitRepositoryAddedSpec] =
    entries.toSet map {e:(UUID,(String,String)) ⇒ GitRepositoryAddedSpec(e _1, e _2)}
  private def toGitFirstRemotesChangedSpecs(entries:Traversable[(UUID,((String,String),(String,String)))]):Set[GitFirstRemoteChangedSpec] =
    entries.toSet map {e:(UUID,((String,String),(String,String))) ⇒ GitFirstRemoteChangedSpec(e _1, e._2._2)}
  private def toGitRemoteAddedSpecs(entries:Traversable[(UUID,String,String)]):Set[GitRemoteAddedSpec] =
    entries.toSet map {e:(UUID,String,String) ⇒ GitRemoteAddedSpec(e _1, (e _2, e _3))}
  private def toGitRemoteRemovedSpecs(entries:Traversable[(UUID,String,String)]):Set[GitRemoteRemovedSpec] =
    entries.toSet map {e:(UUID,String,String) ⇒ GitRemoteRemovedSpec(e _1, e _2)}
  private def toGitRepositoryRemovedSpecs(entries:Traversable[UUID]):Set[GitRepositoryRemovedSpec] =
    entries.toSet map {id:UUID ⇒ GitRepositoryRemovedSpec(id)}
  private def toProjectActiveSignalsAddedSpecs(entries:Traversable[(UUID,Signal[Boolean,OpError,MigrationStyle])]):Set[ProjectActiveSignalAddedSpec] =
    entries.toSet map ProjectActiveSignalAddedSpec.tupled
  private def toProjectActiveSignalsRemovedSpecs(entries:Traversable[(UUID,Signal[Boolean,OpError,MigrationStyle])]):Set[ProjectActiveSignalRemovedSpec] =
    entries.toSet map ProjectActiveSignalRemovedSpec.tupled
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
  gitAdditionalRemotes:Map[UUID,Set[(String,String)]] = Map(),
  gitRepositories:Map[UUID,(String,String)] = Map(),
  projectActiveSignals:Map[UUID,Set[Signal[Boolean,OpError,MigrationStyle]]] = Map(),
  projects:Map[UUID,(UUID,RelativePathSpec)] = Map()
) extends ProjectConfig {
  def activeSignals(projectID:UUID):Set[Signal[Boolean,OpError,MigrationStyle]] = projectActiveSignals.getOrElse(projectID, Set())
  def baseDirectoryIDs:Set[UUID] = baseDirectories keySet
  def baseDirectoryPath(baseDirectoryID:UUID):Option[AbsolutePathSpec] = baseDirectories.get(baseDirectoryID)
  def projectAdditionalGitRemotes(projectID:UUID):Set[(String,String)] = gitAdditionalRemotes.getOrElse(projectID, Set())
  def projectBaseDirectoryID(projectID:UUID):Option[UUID] = projects.get(projectID).map(_._1)
  def projectFirstRemote(projectID:UUID):Option[(String,String)] = gitRepositories.get(projectID)
  def projectIDs:Set[UUID] = projects keySet
  def projectLocalPath(projectID:UUID):Option[RelativePathSpec] = projects.get(projectID).map(_._2)

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

  def addGitRepository(projectID:UUID, firstRemote:(String,String)) =
    if(projects contains projectID)
      if(gitRepositories contains projectID) Left(ProjectAlreadyHasGitRepository(projectID))
      else Right(copy(gitRepositories = gitRepositories + (projectID → firstRemote)))
    else Left(ProjectNotFoundForID(projectID))

  def addGitRepositoryRemote(projectID:UUID, newRemote:(String,String)) =
    if(projects contains projectID)
      if(gitRepositories contains projectID)
        if(gitRepositories(projectID) == newRemote) Left(FirstGitRemoteAlreadyPresent(projectID, newRemote))
        else if(gitAdditionalRemotes.get(projectID).exists(_.contains(newRemote))) Left(AdditionalGitRemoteAlreadyPresent(projectID, newRemote))
        else if(gitRepositories(projectID)._1 == newRemote._1) Left(FirstRemoteWithNameAlreadyPresent(projectID, newRemote _1))
        else if(gitAdditionalRemotes.get(projectID).exists(_.exists(_._1 == newRemote._1))) Left(AdditionalGitRemoteWithNameAlreadyPresent(projectID, newRemote _1))
        else Right(copy(gitAdditionalRemotes = gitAdditionalRemotes + (projectID → (gitAdditionalRemotes.getOrElse(projectID, Set()) + newRemote))))
      else Left(GitRepositoryNotFoundForProjectID(projectID))
    else Left(ProjectNotFoundForID(projectID))

  def addProject(projectID:UUID, baseDirectoryID:UUID, localPath:RelativePathSpec) =
    if(projects contains projectID) Left(ProjectWithSameIDAlreadyExists(projectID))
    else if(!(baseDirectories contains baseDirectoryID)) Left(BaseDirectoryNotFoundForID(baseDirectoryID))
    else if(projects.values exists {p ⇒ (p._1 == baseDirectoryID) && (p._2 == localPath)}) Left(ProjectWithSamePathAlreadyExists(projectID, baseDirectoryID, localPath))
    else if(projects.values exists {p ⇒ (p._1 == baseDirectoryID) && ((p._2 startsWith localPath) || (localPath startsWith p._2))}) Left(ProjectWithCrossingPathAlreadyExists(projectID, baseDirectoryID, localPath))
    else Right(copy(projects = projects + (projectID → (baseDirectoryID, localPath))))

  def addProjectActiveSignal(projectID:UUID, signal:Signal[Boolean,OpError,MigrationStyle]) =
    if(!(projects contains projectID)) Left(ProjectNotFoundForID(projectID))
    else projectActiveSignals get projectID match {
      case None ⇒ Right(copy(projectActiveSignals = projectActiveSignals + (projectID → Set(signal))))
      case Some(containsSignal) if containsSignal(signal) ⇒ Left(ProjectActiveSignalAlreadyPresent(projectID, signal))
      case Some(signals) ⇒ Right(copy(projectActiveSignals = projectActiveSignals + (projectID → (signals + signal))))
    }

  def changeGitFirstRemote(projectID:UUID, newFirstRemote:(String,String)) = gitRepositories get projectID match {
    case None ⇒
      if(!(projects contains projectID)) Left(ProjectNotFoundForID(projectID))
      else Left(GitRepositoryNotFoundForProjectID(projectID))
    case Some(`newFirstRemote`) ⇒ Left(FirstGitRemoteAlreadyPresent(projectID, newFirstRemote))
    case Some(_) ⇒ gitAdditionalRemotes getOrElse (projectID, Set()) match {
      case withNewFirstRemote if withNewFirstRemote contains newFirstRemote ⇒
        Left(AdditionalGitRemoteAlreadyPresent(projectID, newFirstRemote))
      case withNewFirstRemoteName if withNewFirstRemoteName.exists(_._1 == newFirstRemote._1) ⇒
        Left(AdditionalGitRemoteWithNameAlreadyPresent(projectID, newFirstRemote._1))
      case _ ⇒ Right(copy(gitRepositories = gitRepositories + (projectID → newFirstRemote)))
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

  def removeGitRepository(projectID:UUID) =
    if(projects contains projectID)
      if(gitRepositories contains projectID)
        if(gitAdditionalRemotes contains projectID) Left(GitRepositoryStillHasAdditionalRemotes(projectID))
        else Right(copy(gitRepositories = gitRepositories - projectID))
      else Left(GitRepositoryNotFoundForProjectID(projectID))
    else Left(ProjectNotFoundForID(projectID))

  def removeGitRemote(projectID:UUID, remoteName:String) =
    if(projects contains projectID)
      if(gitRepositories contains projectID)
        if(gitRepositories(projectID)._1 == remoteName) Left(CannotRemoveGitFirstRemote(projectID, remoteName))
        else gitAdditionalRemotes get projectID match {
          case None ⇒ Left(GitRemoteNotFoundForName(projectID, remoteName))
          case Some(withoutRemoteName) if !(withoutRemoteName exists (_._1==remoteName)) ⇒ Left(GitRemoteNotFoundForName(projectID, remoteName))
          case Some(withRemoteName) ⇒
            if(withRemoteName.size == 1) Right(copy(gitAdditionalRemotes = gitAdditionalRemotes - projectID))
            else Right(copy(gitAdditionalRemotes = gitAdditionalRemotes + (projectID → gitAdditionalRemotes(projectID).filterNot(_._1 == remoteName))))
        }
      else Left(GitRepositoryNotFoundForProjectID(projectID))
    else Left(ProjectNotFoundForID(projectID))

  def removeProject(projectID:UUID) =
    if(!(projects contains projectID)) Left(ProjectNotFoundForID(projectID))
    else if(gitRepositories contains projectID) Left(ProjectStillReferencedByGitRepository(projectID))
    else if(projectActiveSignals contains projectID) Left(ProjectStillReferencedByActiveSignal(projectID))
    else Right(copy(projects = projects - projectID))

  def removeProjectActiveSignal(projectID:UUID, signal:Signal[Boolean,OpError,MigrationStyle]) =
    if(!(projects contains projectID)) Left(ProjectNotFoundForID(projectID))
    else projectActiveSignals get projectID match {
      case Some(signals) if signals == Set(signal) ⇒ Right(copy(projectActiveSignals = projectActiveSignals - projectID))
      case Some(signals) if signals(signal) ⇒ Right(copy(projectActiveSignals = projectActiveSignals + (projectID → (signals - signal))))
      case _ ⇒ Left(ProjectActiveSignalNotFound(projectID, signal))
    }
}
