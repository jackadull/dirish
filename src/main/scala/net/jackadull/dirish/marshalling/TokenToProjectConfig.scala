package net.jackadull.dirish.marshalling

import java.util.UUID

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.model.ProjectConfig
import net.jackadull.dirish.op.network.IsHostReachable
import net.jackadull.dirish.op.signals.{Signal, SignalCacheConfig}
import net.jackadull.dirish.op.{Op, OpError}
import net.jackadull.dirish.path.{AbsolutePathSpec, PathElementSpec, RelativePathSpec, UserHomePathSpec}

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.language.postfixOps

object TokenToProjectConfig {
  def apply(root:ProjectConfigRootToken):Either[ConfigSemanticError,ProjectConfig] =
    withBaseDirs(root baseDirs, ProjectConfig empty)

  private def withBaseDirs(baseDirs:Seq[BaseDirDefToken], projectConfig:ProjectConfig):Either[ConfigSemanticError,ProjectConfig] =
    baseDirs match {
      case Seq() ⇒ Right(projectConfig)
      case Seq(fst, rst@_*) ⇒ toAbsolutePathSpec(fst directory) match {
        case Right(path) ⇒ projectConfig.addBaseDirectory(fst.id.uuid, path) match {
          case Right(projectConfig2) ⇒ withDirectoryContents(fst.id uuid,  fst contents, projectConfig2, Seq(), Set()) match {
            case Right(projectConfig3) ⇒ withBaseDirs(rst, projectConfig3)
            case Left(err) ⇒ Left(err)
          }
          case Left(err) ⇒ Left(err)
        }
        case Left(err) ⇒ Left(err)
      }
    }

  private def withDirectoryContents(baseDirectoryID:UUID, contents:Seq[DirectoryContentsToken], projectConfig:ProjectConfig, localPaths:Seq[PathElementsToken], activeWhens:Set[ActiveWhenToken], localActiveWhensExtracted:Boolean=false):Either[ConfigSemanticError,ProjectConfig] =
    if(!localActiveWhensExtracted) {
      val (localActiveWhens,otherContents) = extractActiveWhens(contents)
      withDirectoryContents(baseDirectoryID, otherContents, projectConfig, localPaths, activeWhens ++ localActiveWhens, localActiveWhensExtracted = true)
    } else contents match {
      case Seq() ⇒ Right(projectConfig)
      case Seq(DirectoryDefToken(directory, contents2), rst@_*) ⇒
        withDirectoryContents(baseDirectoryID, contents2, projectConfig, localPaths:+directory, activeWhens) match {
          case Right(projectConfig2) ⇒ withDirectoryContents(baseDirectoryID, rst, projectConfig2, localPaths, activeWhens, localActiveWhensExtracted)
          case Left(err) ⇒ Left(err)
        }
      case Seq(project:ProjectDefToken, rst@_*) ⇒ withProject(baseDirectoryID, project, projectConfig, localPaths, activeWhens) match {
        case Right(projectConfig2) ⇒ withDirectoryContents(baseDirectoryID, rst, projectConfig2, localPaths, activeWhens, localActiveWhensExtracted)
        case Left(err) ⇒ Left(err)
      }
    }

  private def extractActiveWhens(contents:Seq[DirectoryContentsToken]):(Set[ActiveWhenToken],Seq[DirectoryContentsToken]) = {
    @tailrec def recurse(rest:Seq[DirectoryContentsToken], foundActiveWhens:Set[ActiveWhenToken], skipped:Seq[DirectoryContentsToken]):(Set[ActiveWhenToken],Seq[DirectoryContentsToken]) =
      rest match {
        case Seq(aw:ActiveWhenToken, r@_*) ⇒ recurse(r, foundActiveWhens + aw, skipped)
        case Seq(x, r@_*) ⇒ recurse(r, foundActiveWhens, skipped :+ x)
        case Seq() ⇒ (foundActiveWhens, skipped)
      }
    recurse(contents, Set(), Seq())
  }

  private def withProject(baseDirectoryID:UUID, project:ProjectDefToken, projectConfig:ProjectConfig, localPaths:Seq[PathElementsToken], activeWhens:Set[ActiveWhenToken]):Either[ConfigSemanticError,ProjectConfig] =
    toRelativePathSpec(localPaths:+project.path) match {
      case Right(projectPath) ⇒ projectConfig.addProject(project.idToken uuid, baseDirectoryID, projectPath) match {
        case Right(projectConfig2) ⇒ withProjectProperties(project.idToken uuid, project.properties, projectConfig2) match {
          case Right(projectConfig3) ⇒ withActiveWhens(project.idToken uuid, activeWhens, projectConfig3)
          case Left(err) ⇒ Left(err)
        }
        case Left(err) ⇒ Left(err)
      }
      case Left(err) ⇒ Left(err)
    }

  private def withProjectProperties(projectID:UUID, props:Traversable[ProjectPropertyToken], projectConfig:ProjectConfig):Either[ConfigSemanticError,ProjectConfig] =
    props.foldLeft[Either[ConfigSemanticError,ProjectConfig]](Right(projectConfig)) {
      case (Right(pc), p) ⇒ withProjectProperty(projectID, p, pc)
      case (Left(err), _) ⇒ Left(err)
    }

  private def withProjectProperty(projectID:UUID, prop:ProjectPropertyToken, projectConfig:ProjectConfig):Either[ConfigSemanticError,ProjectConfig] =
    prop match {
      case p:ActiveWhenToken ⇒ withActiveWhen(projectID, p, projectConfig)
      case p:GitRepositoryDefToken ⇒ withGitRepository(projectID, p, projectConfig)
    }

  private def withGitRepository(projectID:UUID, git:GitRepositoryDefToken, projectConfig:ProjectConfig):Either[ConfigSemanticError,ProjectConfig] = {
    val GitRemoteToken(GitRemoteNameToken(firstRemoteName), GitRemoteURIToken(firstRemoteURI)) = git.remotesToken.remoteTokens.head
    projectConfig.addGitRepository(projectID, (firstRemoteName, firstRemoteURI)) match {
      case Right(projectConfig2) ⇒ withGitRepositoryRemotes(projectID, git.remotesToken.remoteTokens.tail, projectConfig2)
      case Left(err) ⇒ Left(err)
    }
  }

  private def withGitRepositoryRemotes(projectID:UUID, remotes:Seq[GitRemoteToken], projectConfig:ProjectConfig):Either[ConfigSemanticError,ProjectConfig] =
    remotes match {
      case Seq() ⇒ Right(projectConfig)
      case Seq(fst,rst@_*) ⇒
        val GitRemoteToken(GitRemoteNameToken(remoteName), GitRemoteURIToken(remoteURI)) = fst
        projectConfig.addGitRepositoryRemote(projectID, (remoteName, remoteURI)) match {
          case Right(projectConfig2) ⇒ withGitRepositoryRemotes(projectID, rst, projectConfig2)
          case Left(err) ⇒ Left(err)
        }
    }

  private def withActiveWhens(projectID:UUID, activeWhenTokens:Traversable[ActiveWhenToken], projectConfig:ProjectConfig):Either[ConfigSemanticError,ProjectConfig] =
    activeWhenTokens.foldLeft[Either[ConfigSemanticError,ProjectConfig]](Right(projectConfig)) {
      case (Right(pc), aw) ⇒ withActiveWhen(projectID, aw, pc)
      case (Left(err), _) ⇒ Left(err)
    }

  private def withActiveWhen(projectID:UUID, activeWhenToken:ActiveWhenToken, projectConfig:ProjectConfig):Either[ConfigSemanticError,ProjectConfig] =
    activeWhenToken.signals.foldLeft[Either[ConfigSemanticError,ProjectConfig]](Right(projectConfig)) {
      case (Right(pc), sig) ⇒ withActiveSignal(projectID, sig, pc)
      case (Left(err), _) ⇒ Left(err)
    }

  private def withActiveSignal(projectID:UUID, signal:SignalToken, projectConfig:ProjectConfig):Either[ConfigSemanticError,ProjectConfig] =
    projectConfig.addProjectActiveSignal(projectID, toSignal(signal))

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

  // TODO restructure the tokens so they better represent the signal structure
  private def toSignal(token:SignalToken):Signal[Boolean,OpError,MigrationStyle] = token match {
    case CachedSignalToken(uncached, ttl) ⇒
      val duration = toFiniteDuration(ttl)
      Signal(toSignalOp(uncached), SignalCacheConfig(duration, duration))
    case _ ⇒ sys error s"expected CachedSignalToken instead of $token"
  }

  private def toSignalOp(op:SignalToken):Op[Boolean,OpError,MigrationStyle] = op match {
    case HostReachableToken(hostNameToken, within) ⇒ IsHostReachable(hostNameToken hostName, toFiniteDuration(within).toMillis.toInt)
    case _ ⇒ sys error s"unexpected signal op: $op"
  }

  private def toFiniteDuration(durationToken:DurationToken):FiniteDuration =
    durationToken.elements.map({case TimeWithUnitToken(time, unit) ⇒ FiniteDuration(time, unit)}).reduce(_ + _)
}
