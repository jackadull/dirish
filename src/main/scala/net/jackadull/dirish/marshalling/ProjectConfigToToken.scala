package net.jackadull.dirish.marshalling

import java.util.UUID

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.model.ProjectConfig
import net.jackadull.dirish.op.network.IsHostReachable
import net.jackadull.dirish.op.signals.Signal
import net.jackadull.dirish.op.{Op, OpError}
import net.jackadull.dirish.path._

import scala.concurrent.duration.Duration
import scala.language.postfixOps

object ProjectConfigToToken {
  def apply(projectConfig:ProjectConfig):ProjectConfigRootToken =
    ProjectConfigRootToken(projectConfig.baseDirectoryIDs.toList.sorted map {id ⇒ toBaseDirDef(id, projectConfig)})

  private def toBaseDirDef(baseDirectoryID:UUID, projectConfig:ProjectConfig):BaseDirDefToken = BaseDirDefToken(
    toPathElements(projectConfig.baseDirectoryPath(baseDirectoryID) get),
    UUIDToken(baseDirectoryID),
    projectConfig.projectIDs.filter(pid ⇒ projectConfig.projectBaseDirectoryID(pid) contains baseDirectoryID).toList.sorted map {pid ⇒ toDirectoryContents(pid, projectConfig)}
  )

  private def toDirectoryContents(projectID:UUID, projectConfig:ProjectConfig):DirectoryContentsToken = ProjectDefToken(
    toPathElements(projectConfig.projectLocalPath(projectID) get),
    UUIDToken(projectID),
    toProjectProperties(projectID, projectConfig)
  )

  private def toProjectProperties(projectID:UUID, projectConfig:ProjectConfig):Seq[ProjectPropertyToken] =
    toActiveWhenTokenOpt(projectID, projectConfig).toSeq ++ toGitRepositoryDefOpt(projectID, projectConfig).toSeq

  private def toActiveWhenTokenOpt(projectID:UUID, projectConfig:ProjectConfig):Option[ActiveWhenToken] = {
    val activeSignals = projectConfig.activeSignals(projectID)
    if(activeSignals isEmpty) None
    else Some(ActiveWhenToken(activeSignals.toList.map(toSignal)))
  }

  private def toSignal(signal:Signal[Boolean,OpError,MigrationStyle]):SignalToken =
    CachedSignalToken(toSignal(signal get), toDuration(signal.caching cacheDuration))

  private def toSignal(op:Op[Boolean,OpError,MigrationStyle]):SignalToken = op match {
    case IsHostReachable(host, timeoutMillis) ⇒
      HostReachableToken(HostNameToken(host), DurationToken(Seq(TimeWithUnitToken(timeoutMillis, "ms"))))
    case _ ⇒ sys error s"unknown signal operation: $op"
  }

  private def toDuration(duration:Duration):DurationToken =
    DurationToken(Seq(TimeWithUnitToken(duration.toMillis.toInt, "ms")))

  private def toGitRepositoryDefOpt(projectID:UUID, projectConfig:ProjectConfig):Option[GitRepositoryDefToken] =
    projectConfig.projectFirstRemote(projectID) map {
      case (firstRemoteName, firstRemoteURI) ⇒ GitRepositoryDefToken(GitRemotesToken(
        List(GitRemoteToken(GitRemoteNameToken(firstRemoteName), GitRemoteURIToken(firstRemoteURI))) ++
          additionalGitRemotes(projectID, projectConfig)
      ))
    }

  private def additionalGitRemotes(projectID:UUID, projectConfig:ProjectConfig):List[GitRemoteToken] =
    projectConfig.projectAdditionalGitRemotes(projectID).toList.sortBy(_ _1) map {case (name, uri) ⇒
      GitRemoteToken(GitRemoteNameToken(name), GitRemoteURIToken(uri))
    }

  private def toPathElements(path:AbsolutePathSpec):PathElementsToken = path match {
    case CompositeAbsolutePathSpec(parent, child) ⇒
      val PathElementsToken(parentList) = toPathElements(parent)
      PathElementsToken(parentList :+ PathElementToken(child name))
    case _:AbsolutePathBase ⇒ PathElementsToken(List(PathElementToken(path toString)))
  }

  private def toPathElements(path:RelativePathSpec):PathElementsToken = path match {
    case CompositeRelativePathSpec(parent, child) ⇒
      val PathElementsToken(parentList) = toPathElements(parent)
      PathElementsToken(parentList :+ PathElementToken(child name))
    case PathElementSpec(name) ⇒ PathElementsToken(List(PathElementToken(name)))
  }
}
