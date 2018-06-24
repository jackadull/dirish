package net.jackadull.dirish.op

import java.nio.charset.Charset

import net.jackadull.dirish.op.TestStyle._
import net.jackadull.dirish.op.combinator.{BlockingEitherCombinatorStyle, CombinatorStyle, EitherV}
import net.jackadull.dirish.op.git.{GenericGitError, GitStyle}
import net.jackadull.dirish.op.io._
import net.jackadull.dirish.op.log.Log.OpLogLevel
import net.jackadull.dirish.op.log.LogStyle
import net.jackadull.dirish.op.network.{CanConnectToHostError, IsHostReachableError, NetworkStyle}
import net.jackadull.dirish.op.settings._
import net.jackadull.dirish.op.signals.SignalStyle.SignalGet
import net.jackadull.dirish.op.signals.{SignalCacheConfig, SignalStyle}
import net.jackadull.dirish.path.{AbsolutePathSpec, CompositeAbsolutePathSpec, UserHomePathSpec}

import scala.language.{postfixOps, reflectiveCalls}

class TestStyle extends DefaultDirishSettingStyle[EitherV] with BlockingEitherCombinatorStyle with GitStyle[EitherV] with IOStyle[EitherV]
with LogStyle[EitherV] with NetworkStyle[EitherV] with SignalStyle[EitherV] {
  protected def combinatorStyle:CombinatorStyle[EitherV] = BlockingEitherCombinatorStyle

  def clearSignalCache() {mutex synchronized {signalCache = Map()}}
  def logMessages:Seq[(OpLogLevel,String,Option[Throwable])] = mutex synchronized _logMessages
  def setHostReachable(host:String, reachable:Boolean) =
    if(reachable) mutex synchronized {unreachableHosts -= host} else mutex synchronized {unreachableHosts += host}

  def addGitRemote(path:AbsolutePathSpec,remoteName:String, uri:String):EitherV[Unit,GenericGitError] =
    visitFileNodeAt(path) {
      case None ⇒ (None, Left(GenericMessageError(s"Git directory not found: $path")))
      case Some(dir:TestDirectoryNode) ⇒ dir gitRemotes match {
        case None ⇒ (None, Left(GenericMessageError(s"Not a Git repository at: $path")))
        case Some(remotes) ⇒
          val newRemotes = remotes.filterNot(_._1 == remoteName) :+ (remoteName → uri)
          (Some(dir.copy(gitRemotes = Some(newRemotes))), Right(()))
      }
      case Some(_) ⇒ (None, Left(GenericMessageError(s"Git directory is a regular file: $path")))
    }

  def canConnectToHost(host:String, port:Int, timeoutMillis:Int):EitherV[Boolean,CanConnectToHostError] =
    mutex synchronized {Right(!unreachableHosts(host))}

  def cloneGitRepository(path:AbsolutePathSpec, remoteName:String, uri:String):EitherV[Unit,GenericGitError] =
    path match {
      case UserHomePathSpec ⇒ Left(GenericMessageError("cannot clone into user home"))
      case CompositeAbsolutePathSpec(parent, child) ⇒ visitFileNodeAt(parent) {
        case None ⇒ (None, Left(GenericMessageError(s"parent directory $parent not found")))
        case Some(parentDir:TestDirectoryNode) ⇒ parentDir.children get child.name match {
          case None ⇒ (
            Some(parentDir.copy(children = parentDir.children + (child.name → TestDirectoryNode(
              children = Map(".git" → TestPlainFileNode(Vector())),
              gitRemotes = Some(Seq(remoteName → uri))
            )))),
            Right(())
          )
          case Some(childDir:TestDirectoryNode) ⇒ childDir.gitRemotes match {
            case None ⇒ (
              Some(
                parentDir.copy(children = parentDir.children + (child.name → childDir.copy(
                  gitRemotes = Some(Seq(remoteName→uri)),
                  children = childDir.children + (".git" → TestPlainFileNode(Vector()))
                )))
              ),
              Right(())
            )
            case Some(_) ⇒ (None, Left(GenericMessageError(s"already a Git module: $path")))
          }
          case Some(_) ⇒ (None, Left(GenericMessageError(s"target already exists and is not a directory: $path")))
        }
        case Some(_) ⇒ (None, Left(GenericMessageError(s"parent is not a directory: $parent")))
      }
    }

  def createDirectories(path:AbsolutePathSpec):EitherV[Unit,CreateDirectoryError] =
    fileKindInfo(path) match {
      case Left(error) ⇒ Left(GenericMessageError(s"Cannot get file kind info on $path: $error"))
      case Right(IsNonExistent) ⇒ path match {
        case UserHomePathSpec ⇒ Left(GenericMessageError("Corrupt file structure: User home does not exist."))
        case CompositeAbsolutePathSpec(parent, _) ⇒ createDirectories(parent) match {
          case Left(error) ⇒ Left(error)
          case Right(_) ⇒ createDirectory(path)
        }
      }
      case Right(IsDirectory) ⇒ Right(())
      case Right(IsRegularFile) ⇒ Left(GenericMessageError(s"Cannot create directory because regular file exists at: $path"))
    }

  def createDirectory(path:AbsolutePathSpec):EitherV[Unit,CreateDirectoryError] = path match {
    case UserHomePathSpec ⇒ Left(GenericMessageError("Cannot create the user home directory"))
    case CompositeAbsolutePathSpec(parent, dirChild) ⇒
      def recurse(p:AbsolutePathSpec):EitherV[Unit,CreateDirectoryError] = p match {
        case UserHomePathSpec ⇒ Right(())
        case CompositeAbsolutePathSpec(par, ch) ⇒
          recurse(par)
          visitFileNodeAt(par) {
            case None ⇒ (None, Left(GenericMessageError(s"Cannot create parent directory $par")))
            case Some(_:TestPlainFileNode) ⇒ (None, Left(GenericMessageError(s"Parent is a plain file: $par")))
            case Some(dir:TestDirectoryNode) ⇒ dir.children get ch.name match {
              case Some(_) ⇒ (None, Right(()))
              case None ⇒ (Some(dir.copy(children = dir.children + (ch.name → TestDirectoryNode()))), Right(()))
            }
          }
      }
      recurse(parent) match {
        case Right(()) ⇒ visitFileNodeAt(parent) {
          case Some(dir:TestDirectoryNode) ⇒ dir.children get dirChild.name  match {
            case None ⇒ (Some(dir.copy(children = dir.children + (dirChild.name → TestDirectoryNode()))), Right(()))
            case Some(_) ⇒ (None, Left(GenericMessageError(s"Target already exists: $path")))
          }
          case _ ⇒ (None, Left(GenericMessageError(s"Unable to create parent directory $parent")))
        }
        case Left(err) ⇒ Left(err)
        case unexpected ⇒ Left(GenericMessageError(s"Unexpected: $unexpected"))
      }
  }

  def createLockFile(path:AbsolutePathSpec):EitherV[Unit,CreateLockFileError] = path match {
    case UserHomePathSpec ⇒ Left(GenericMessageError("User home cannot be a lock file"))
    case CompositeAbsolutePathSpec(parent, child) ⇒ visitFileNodeAt(parent) {
      case None ⇒ (None, Left(GenericMessageError(s"Parent directory not found: $parent")))
      case Some(parentDir:TestDirectoryNode) ⇒ parentDir.children get child.name match {
        case None ⇒ (
          Some(parentDir.copy(children = parentDir.children + (child.name → TestPlainFileNode(Vector())))), Right(())
        )
        case Some(_) ⇒ (None, Left(FileAlreadyExists("Cannot create lock file", path toString)))
      }
      case Some(_) ⇒ (None, Left(GenericMessageError(s"Parent is not a directory: $parent")))
    }
  }

  def deleteFile(path:AbsolutePathSpec):EitherV[Unit,DeleteFileError] = path match {
    case UserHomePathSpec ⇒ Left(GenericMessageError("Cannot remove user home"))
    case CompositeAbsolutePathSpec(parent, child) ⇒ visitFileNodeAt(parent) {
      case Some(parentDir:TestDirectoryNode) ⇒ parentDir.children get child.name match {
        case None ⇒ (None, Left(NoSuchFile("Cannot delete file because it does not exist", path toString)))
        case Some(_:TestPlainFileNode) ⇒ (Some(parentDir.copy(children = parentDir.children - child.name)), Right(()))
        case Some(f:TestDirectoryNode) ⇒
          if(f.children nonEmpty) (None, Left(GenericMessageError("directory not empty: $path")))
          else (Some(parentDir.copy(children = parentDir.children - child.name)), Right(()))
      }
      case _ ⇒ (None, Left(NoSuchFile("Cannot delete file because it does not exist", path toString)))
    }
  }

  def fileKindInfo(path:AbsolutePathSpec):EitherV[FileKind,FileKindInfoError] = visitFileNodeAt(path) {
    case Some(_:TestDirectoryNode) ⇒ (None, Right(IsDirectory))
    case Some(_:TestPlainFileNode) ⇒ (None, Right(IsRegularFile))
    case None ⇒ (None, Right(IsNonExistent))
  }

  def hasLocalGitChanges(path:AbsolutePathSpec):EitherV[Boolean,GenericGitError] = visitFileNodeAt(path) {
    case None ⇒ (None, Left(GenericMessageError(s"Git directory not found: $path")))
    case Some(dir:TestDirectoryNode) ⇒ dir gitRemotes match {
      case None ⇒ (None, Left(GenericMessageError(s"Not a Git repository: $path")))
      case Some(_) ⇒ (None, Right((dir.children.keySet - ".git") nonEmpty))
    }
    case Some(_) ⇒ (None, Left(GenericMessageError(s"Not a directory: $path")))
  }

  def isHostReachable(host:String, timeoutMillis:Int):EitherV[Boolean,IsHostReachableError] =
    mutex synchronized {Right(!unreachableHosts(host))}

  def listDirectory(path:AbsolutePathSpec):EitherV[Set[AbsolutePathSpec],ListDirectoryError] = visitFileNodeAt(path) {
    case None ⇒ (None, Left(NoSuchFile("Directory not found", path toString)))
    case Some(dir:TestDirectoryNode) ⇒ (None, Right(dir.children.toSeq map {
      case (childName, _) ⇒ path/childName
    } toSet))
    case Some(_) ⇒ (None, Left(NotADirectory(s"Not a directory", path toString)))
  }

  def log(logger:String, level:OpLogLevel, message:String, throwableOpt:Option[Throwable]):EitherV[Unit,Nothing] = mutex synchronized {
    _logMessages :+= (level, message, throwableOpt)
    Right(())
  }

  def moveFile(source:AbsolutePathSpec, target:AbsolutePathSpec):EitherV[Unit,MoveFileError] = mutex synchronized {
    if(source == target) Left(GenericMessageError(s"Source and target path are identical: $source"))
    else if(source startsWith target) Left(GenericMessageError("Cannot move a file to one of its parents"))
    else if(target startsWith source) Left(GenericMessageError("Cannot move a file into one of its subdirectories"))
    else target match {
      case UserHomePathSpec ⇒ Left(GenericMessageError("Cannot replace user home"))
      case CompositeAbsolutePathSpec(targetParent, targetChild) ⇒
        source match {
          case UserHomePathSpec ⇒ Left(GenericMessageError("Cannot move user home directory"))
          case CompositeAbsolutePathSpec(sourceParent, sourceChild) ⇒
            val sourceFound:Either[MoveFileError,TestFileNode] = visitFileNodeAt(source) {
              case None ⇒ (None, Left(NoSuchFile("Source file not found", source toString)))
              case Some(sourceNode) ⇒ (None, Right(sourceNode))
            }
            val wroteToTarget:Either[MoveFileError,Unit] = sourceFound match {
              case Left(r) ⇒ Left(r)
              case Right(sourceNode) ⇒ visitFileNodeAt(targetParent) {
                case None ⇒ (None, Left(GenericMessageError(s"Parent directory of target path not found: $targetParent")))
                case Some(targetParentNode:TestDirectoryNode) ⇒ targetParentNode.children get targetChild.name match {
                  case Some(_) ⇒ (None, Left(FileAlreadyExists("Target file already exists", target toString)))
                  case None ⇒ (Some(targetParentNode.copy(children = targetParentNode.children + (targetChild.name → sourceNode))), Right(()))
                }
                case Some(_) ⇒ (None, Left(GenericMessageError(s"Parent of target path is not a directory: $targetParent")))
              }
            }
            wroteToTarget match {
              case Left(r) ⇒ Left(r)
              case Right(_) ⇒ visitFileNodeAt(sourceParent) {
                case Some(sourceParentNode:TestDirectoryNode) ⇒
                  (Some(sourceParentNode.copy(children = sourceParentNode.children - sourceChild.name)), Right(()))
                case _ ⇒ sys error "internal error"
              }
            }
        }
    }
  }

  def moveFileToTrash(path:AbsolutePathSpec):EitherV[Unit,MoveToTrashError] = path match {
    case UserHomePathSpec ⇒ Left(GenericMessageError("Cannot move user home to trash"))
    case CompositeAbsolutePathSpec(parent, child) ⇒ visitFileNodeAt(parent) {
      case Some(parentDir:TestDirectoryNode) ⇒ parentDir.children get child.name match {
        case None ⇒ (None, Left(NoSuchFile("File not found", parent toString)))
        case Some(_) ⇒ (Some(parentDir.copy(children = parentDir.children - child.name)), Right(()))
      }
      case _ ⇒ (None, Left(NoSuchFile("Parent directory not found", parent toString)))
    }
  }

  def pullGitRepository(path:AbsolutePathSpec):EitherV[Unit,GenericGitError] = ???

  def readFileAsString(path:AbsolutePathSpec, charset:Charset):EitherV[String,ReadFileAsStringError] = visitFileNodeAt(path) {
    case Some(f:TestPlainFileNode) ⇒ (None, Right(new String(f.contents toArray, charset)))
    case Some(_) ⇒ (None, Left(GenericMessageError(s"not a plain file: $path")))
    case None ⇒ (None, Left(NoSuchFile("File not found", path toString)))
  }

  def removeGitRemote(path:AbsolutePathSpec, remoteName:String):EitherV[Unit, GenericGitError] =
    visitFileNodeAt(path) {
      case None ⇒ (None, Left(GenericMessageError(s"Git directory not found: $path")))
      case Some(dir:TestDirectoryNode) ⇒ dir.gitRemotes match {
        case None ⇒ (None, Left(GenericMessageError(s"not a Git repository: $path")))
        case Some(remotes) if !(remotes exists {_._1 == remoteName}) ⇒ (None, Left(GenericMessageError(s"Git remote not found: $remoteName")))
        case Some(remotes) ⇒ (Some(dir.copy(gitRemotes = Some(remotes.filterNot(_._1 == remoteName)))), Right(()))
      }
      case Some(_) ⇒ (None, Left(GenericMessageError(s"Not a directory: $path")))
    }

  def removeGitRepository(path:AbsolutePathSpec):EitherV[Unit, GenericGitError] = visitFileNodeAt(path) {
    case None ⇒ (None, Left(GenericMessageError(s"Git directory not found: $path")))
    case Some(dir:TestDirectoryNode) ⇒ dir.gitRemotes match {
      case None ⇒ (None, Left(GenericMessageError(s"not a Git repository: $path")))
      case Some(_) ⇒ (Some(dir.copy(gitRemotes = None, children = dir.children - ".git")), Right(()))
    }
    case Some(_) ⇒ (None, Left(GenericMessageError(s"Not a directory: $path")))
  }

  def saveStringToFile(path:AbsolutePathSpec, str:String, charset:Charset):EitherV[Unit,SaveStringToFileError] = path match {
    case UserHomePathSpec ⇒ Left(GenericMessageError("cannot create user home as file"))
    case CompositeAbsolutePathSpec(parent, child) ⇒ visitFileNodeAt(parent) {
      case None ⇒ (None, Left(GenericMessageError(s"parent directory not found: $parent")))
      case Some(parentDir:TestDirectoryNode) ⇒ parentDir.children get child.name match {
        case None | Some(_:TestPlainFileNode) ⇒
          (Some(parentDir.copy(children = parentDir.children + (child.name → TestPlainFileNode(str.getBytes(charset) toVector)))), Right(()))
        case _ ⇒ (None, Left(GenericMessageError(s"directory exists at target: $path")))
      }
      case Some(_) ⇒ (None, Left(GenericMessageError(s"parent is not a directory: $parent")))
    }
  }

  def signal[R,E](get:SignalGet[R,E,EitherV], caching:SignalCacheConfig):EitherV[R,E] = mutex synchronized {signalCache get get} match {
    case Some(cached) ⇒ cached.asInstanceOf[EitherV[R,E]]
    case None ⇒
      val result = get()
      mutex synchronized {signalCache += (get → result)}
      result
  }

  private val mutex = new Object
  private var signalCache:Map[AnyRef,EitherV[_,_]] = Map()
  private var fsRoot:TestFileNode = TestDirectoryNode()
  private var _logMessages:Seq[(OpLogLevel,String,Option[Throwable])] = Seq()
  private var unreachableHosts:Set[String] = Set()
  private def visitFileNodeAt[A](location:AbsolutePathSpec)(f:Option[TestFileNode]⇒(Option[TestFileNode],A)):A = mutex synchronized {
    location match {
      case UserHomePathSpec ⇒ val (newRoot, result) = f(Some(fsRoot)); newRoot foreach {r ⇒ fsRoot = r}; result
      case CompositeAbsolutePathSpec(parent, child) ⇒ visitFileNodeAt(parent) {
        case None ⇒ f(None)
        case Some(parentDir:TestDirectoryNode) ⇒ parentDir.children get child.name match {
          case None ⇒ f(None)
          case Some(childNode) ⇒ f(Some(childNode)) match {
            case (None,result) ⇒ (None,result)
            case (Some(newChild), result) ⇒
              (Some(parentDir.copy(children = parentDir.children + (child.name → newChild))), result)
          }
        }
        case Some(_) ⇒ f(None)
      }
    }
  }
}
object TestStyle {
  private[op] sealed trait TestFileNode
  private[op] final case class TestDirectoryNode(children:Map[String,TestFileNode]=Map(), gitRemotes:Option[Seq[(String,String)]]=None) extends TestFileNode
  private[op] final case class TestPlainFileNode(contents:Vector[Byte]) extends TestFileNode
}
