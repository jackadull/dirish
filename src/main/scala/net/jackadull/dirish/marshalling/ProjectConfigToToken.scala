package net.jackadull.dirish.marshalling

import java.util.UUID

import net.jackadull.dirish.io.flags.{CachedIOFlag, IOFlag, IsHostReachableFlag}
import net.jackadull.dirish.model.ProjectConfig
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
    toActiveWhenTokenOpt(projectID, projectConfig).toSeq ++ toGitModuleDefOpt(projectID, projectConfig).toSeq

  private def toActiveWhenTokenOpt(projectID:UUID, projectConfig:ProjectConfig):Option[ActiveWhenToken] = {
    val activeFlags = projectConfig.activeFlagsOfProject(projectID)
    if(activeFlags isEmpty) None
    else Some(ActiveWhenToken(activeFlags.toList.map(toFlag)))
  }

  private def toFlag(flag:IOFlag):FlagToken = flag match {
    case CachedIOFlag(uncached, ttl) ⇒ CachedFlagToken(toFlag(uncached), toDuration(ttl))
    case IsHostReachableFlag(host, timeoutMillis) ⇒ HostReachableToken(HostNameToken(host), DurationToken(Seq(TimeWithUnitToken(timeoutMillis, "ms"))))
  }

  private def toDuration(duration:Duration):DurationToken =
    DurationToken(Seq(TimeWithUnitToken(duration.toMillis.toInt, "ms")))

  private def toGitModuleDefOpt(projectID:UUID, projectConfig:ProjectConfig):Option[GitModuleDefToken] =
    projectConfig.projectFirstRemote(projectID) map {
      case (firstRemoteName, firstRemoteURI) ⇒ GitModuleDefToken(GitRemotesToken(
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
