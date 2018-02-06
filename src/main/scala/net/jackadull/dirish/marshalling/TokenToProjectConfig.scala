package net.jackadull.dirish.marshalling

import java.util.UUID

import net.jackadull.dirish.model.ProjectConfig
import net.jackadull.dirish.path.{AbsolutePathSpec, PathElementSpec, RelativePathSpec, UserHomePathSpec}

import scala.language.postfixOps

object TokenToProjectConfig {
  def apply(root:ProjectConfigRootToken):Either[ConfigSemanticError,ProjectConfig] =
    withBaseDirs(root baseDirs, ProjectConfig empty)

  private def withBaseDirs(baseDirs:Seq[BaseDirDefToken], projectConfig:ProjectConfig):Either[ConfigSemanticError,ProjectConfig] =
    baseDirs match {
      case Seq() ⇒ Right(projectConfig)
      case Seq(fst, rst@_*) ⇒ toAbsolutePathSpec(fst directory) match {
        case Right(path) ⇒ projectConfig.addBaseDirectory(fst.id.uuid, path) match {
          case Right(projectConfig2) ⇒ withDirectoryContents(fst.id uuid,  fst contents, projectConfig2, Seq()) match {
            case Right(projectConfig3) ⇒ withBaseDirs(rst, projectConfig3)
            case Left(err) ⇒ Left(err)
          }
          case Left(err) ⇒ Left(err)
        }
        case Left(err) ⇒ Left(err)
      }
    }

  private def withDirectoryContents(baseDirectoryID:UUID, contents:Seq[DirectoryContentsToken], projectConfig:ProjectConfig, localPaths:Seq[PathElementsToken]):Either[ConfigSemanticError,ProjectConfig] =
    contents match {
      case Seq() ⇒ Right(projectConfig)
      case Seq(DirectoryDefToken(directory, contents2), rst@_*) ⇒
        withDirectoryContents(baseDirectoryID, contents2, projectConfig, localPaths:+directory) match {
          case Right(projectConfig2) ⇒ withDirectoryContents(baseDirectoryID, rst, projectConfig2, localPaths)
          case Left(err) ⇒ Left(err)
        }
      case Seq(project:ProjectDefToken, rst@_*) ⇒ withProject(baseDirectoryID, project, projectConfig, localPaths) match {
        case Right(projectConfig2) ⇒ withDirectoryContents(baseDirectoryID, rst, projectConfig2, localPaths)
        case Left(err) ⇒ Left(err)
      }
    }

  private def withProject(baseDirectoryID:UUID, project:ProjectDefToken, projectConfig:ProjectConfig, localPaths:Seq[PathElementsToken]):Either[ConfigSemanticError,ProjectConfig] =
    toRelativePathSpec(localPaths:+project.path) match {
      case Right(projectPath) ⇒ projectConfig.addProject(project.idToken uuid, baseDirectoryID, projectPath) match {
        case Right(projectConfig2) ⇒ project gitModuleDefOpt match {
          case Some(git) ⇒ withGitModule(project.idToken uuid, git, projectConfig2)
          case None ⇒ Right(projectConfig2)
        }
        case Left(err) ⇒ Left(err)
      }
      case Left(err) ⇒ Left(err)
    }

  private def withGitModule(projectID:UUID, git:GitModuleDefToken, projectConfig:ProjectConfig):Either[ConfigSemanticError,ProjectConfig] = {
    val GitRemoteToken(GitRemoteNameToken(firstRemoteName), GitRemoteURIToken(firstRemoteURI)) = git.remotesToken.remoteTokens.head
    projectConfig.addGitModule(projectID, (firstRemoteName, firstRemoteURI)) match {
      case Right(projectConfig2) ⇒ withGitModuleRemotes(projectID, git.remotesToken.remoteTokens.tail, projectConfig2)
      case Left(err) ⇒ Left(err)
    }
  }

  private def withGitModuleRemotes(projectID:UUID, remotes:Seq[GitRemoteToken], projectConfig:ProjectConfig):Either[ConfigSemanticError,ProjectConfig] =
    remotes match {
      case Seq() ⇒ Right(projectConfig)
      case Seq(fst,rst@_*) ⇒
        val GitRemoteToken(GitRemoteNameToken(remoteName), GitRemoteURIToken(remoteURI)) = fst
        projectConfig.addGitModuleRemote(projectID, (remoteName, remoteURI)) match {
          case Right(projectConfig2) ⇒ withGitModuleRemotes(projectID, rst, projectConfig2)
          case Left(err) ⇒ Left(err)
        }
    }

  private def toAbsolutePathSpec(elements:PathElementsToken):Either[ConfigSemanticError,AbsolutePathSpec] = elements.elements match {
    case Seq(PathElementToken("$HOME"), rst@_*) ⇒ Right(rst.foldLeft[AbsolutePathSpec](UserHomePathSpec) {_ / _.name})
    case Seq(fst, _*) ⇒ Left(InvalidAbsolutePathBegin(fst))
    case Seq() ⇒ Left(EmptyAbsolutePath)
  }

  private def toRelativePathSpec(localPaths:Seq[PathElementsToken]):Either[ConfigSemanticError,RelativePathSpec] = localPaths match {
    case Seq() ⇒ Left(EmptyRelativePath)
    case Seq(only) ⇒ toRelativePathSpec(only)
    case Seq(fst,rst@_*) ⇒ toRelativePathSpec(rst) match {
      case Right(rstPath) ⇒ toRelativePathSpec(fst) match {
        case Right(fstPath) ⇒ Right(fstPath / rstPath)
        case Left(err) ⇒ Left(err)
      }
      case Left(err) ⇒ Left(err)
    }
  }

  private def toRelativePathSpec(elements:PathElementsToken):Either[ConfigSemanticError,RelativePathSpec] =
    toRelativePathSpec2(elements elements)

  private def toRelativePathSpec2(elements:Seq[PathElementToken]):Either[ConfigSemanticError,RelativePathSpec] = elements match {
    case Seq() ⇒ Left(EmptyRelativePath)
    case Seq(only) ⇒ Right(PathElementSpec(only name))
    case Seq(fst,rst@_*) ⇒ toRelativePathSpec2(rst) match {
      case Right(rstPath) ⇒ Right(PathElementSpec(fst name) / rstPath)
      case Left(err) ⇒ Left(err)
    }
  }
}
