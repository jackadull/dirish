package net.jackadull.dirish.marshalling

import java.util.UUID

import net.jackadull.dirish.model.ProjectConfig
import net.jackadull.dirish.path._

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
    toGitModuleDefOpt(projectID, projectConfig)
  )

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
