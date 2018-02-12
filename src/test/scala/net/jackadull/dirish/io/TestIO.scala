package net.jackadull.dirish.io
import java.io.{FileNotFoundException, IOException}
import java.nio.charset.Charset

import net.jackadull.dirish.io.TestIO.{TestIODirectoryNode, TestIOFileNode, TestIOPlainFileNode}
import net.jackadull.dirish.path.{AbsolutePathSpec, CompositeAbsolutePathSpec, UserHomePathSpec}

import scala.language.postfixOps

sealed trait TestIOV[+A] {
  private[io] lazy val value:A = execute()
  protected def execute():A
}

class TestIO extends IO[TestIOV] {
  def logMessages:Seq[(LogCategory,String,Option[Throwable])] = mutex synchronized {_logMessages}

  def markForExecution[A](a:TestIOV[A]):TestIOV[Unit] = {a value; v(())}

  def aggregate[A,B](as:Traversable[TestIOV[A]])(z: ⇒B)(seqop:(B,A)⇒B, combop:(B,B)⇒B):TestIOV[B] =
    v {(as map {_ value}).aggregate(z)(seqop, combop)}
  def bind[A](a: ⇒A):TestIOV[A] = v(a)
  def flatMap[A,B](a:TestIOV[A])(f:A⇒TestIOV[B]):TestIOV[B] = v {f(a.value).value}
  def flatten[A](a:TestIOV[TestIOV[A]]):TestIOV[A] = v {a.value.value}
  def map[A, B](a:TestIOV[A])(f:A ⇒ B):TestIOV[B] = v {f(a value)}

  def addGitModuleRemote(gitModulePath:AbsolutePathSpec, remoteName:String, remoteURI:String):TestIOV[AddGitModuleRemoteResult] =
    v {visitFileNodeAt(gitModulePath) {
      case None ⇒ (None, GenericIOError(new FileNotFoundException(gitModulePath toString)))
      case Some(dir:TestIODirectoryNode) ⇒ dir.gitRemotes match {
        case None ⇒ (None, GenericIOError(new IOException(s"not a Git module $gitModulePath")))
        case Some(remotes) ⇒
          val newRemotes = remotes.filterNot(_._1 == remoteName) :+ (remoteName → remoteURI)
          (Some(dir.copy(gitRemotes = Some(newRemotes))), IOSuccess)
      }
      case Some(_) ⇒ (None, GenericIOError(new FileNotFoundException(s"not a directory: $gitModulePath")))
    }}

  def cloneGitModule(gitModulePath:AbsolutePathSpec, remoteName:String, remoteURI:String):TestIOV[CloneGitModuleResult] =
    gitModulePath match {
      case UserHomePathSpec ⇒ v {GenericIOError(new IOException(s"cannot clone into user home"))}
      case CompositeAbsolutePathSpec(parent, child) ⇒ v{ visitFileNodeAt(parent) {
        case None ⇒ (None, GenericIOError(new FileNotFoundException(s"parent directory $parent not found")))
        case Some(parentDir:TestIODirectoryNode) ⇒ parentDir.children get child.name match {
          case None ⇒ (
            Some(parentDir.copy(children = parentDir.children + (child.name → TestIODirectoryNode(
              children = Map(".git" → TestIOPlainFileNode(Vector())),
              gitRemotes = Some(Seq(remoteName → remoteURI))
            )))),
            IOSuccess
          )
          case Some(childDir:TestIODirectoryNode) ⇒ childDir.gitRemotes match {
            case None ⇒ (
              Some(
                parentDir.copy(children = parentDir.children + (child.name → childDir.copy(
                  gitRemotes = Some(Seq(remoteName→remoteURI)),
                  children = childDir.children + (".git" → TestIOPlainFileNode(Vector()))
                )))
              ),
              IOSuccess
            )
            case Some(_) ⇒ (None, GenericIOError(new IOException(s"already a Git module: $gitModulePath")))
          }
          case Some(_) ⇒ (None, GenericIOError(new IOException(s"target already exists and is not a directory: $gitModulePath")))
        }
        case Some(_) ⇒ (None, GenericIOError(new FileNotFoundException(s"parent is not a directory: $parent")))
      }}
    }

  def createDirectory(directoryPath:AbsolutePathSpec):TestIOV[CreateDirectoryResult] = directoryPath match {
    case UserHomePathSpec ⇒ v {GenericIOError(new IOException("cannot create the user home directory"))}
    case CompositeAbsolutePathSpec(parent, dirChild) ⇒
      def recurse(p:AbsolutePathSpec):IOResult = {p match {
        case UserHomePathSpec ⇒ IOSuccess
        case CompositeAbsolutePathSpec(par, ch) ⇒
          recurse(par)
          visitFileNodeAt(par) {
            case None ⇒ (None, GenericIOError(new IOException(s"cannot create parent directory $par")))
            case Some(_:TestIOPlainFileNode) ⇒ (None, GenericIOError(new IOException(s"parent is a plain file: $par")))
            case Some(dir:TestIODirectoryNode) ⇒ dir.children get ch.name match {
              case Some(_) ⇒ (None, IOSuccess)
              case None ⇒ (Some(dir.copy(children = dir.children + (ch.name → TestIODirectoryNode()))), IOSuccess)
            }
          }
      }}
      v {recurse(parent) match {
        case IOSuccess ⇒ visitFileNodeAt(parent) {
          case Some(dir:TestIODirectoryNode) ⇒ dir.children get dirChild.name  match {
            case None ⇒ (Some(dir.copy(children = dir.children + (dirChild.name → TestIODirectoryNode()))), IOSuccess)
            case Some(_) ⇒ (None, GenericIOError(new IOException(s"target already exists: $directoryPath")))
          }
          case _ ⇒ (None, GenericIOError(new IOException(s"unable to create parent directory $parent")))
        }
        case err:GenericIOError ⇒ err
        case unexpected ⇒ GenericIOError(new IOException(s"unexpected: $unexpected"))
      }}
    }

  def createLockFile(path:AbsolutePathSpec):TestIOV[CreateLockFileResult] = path match {
    case UserHomePathSpec ⇒ v {GenericIOError(new IOException("user home cannot be a lock file"))}
    case CompositeAbsolutePathSpec(parent, child) ⇒ v {visitFileNodeAt(parent) {
      case None ⇒ (None, GenericIOError(new FileNotFoundException(s"parent directory not found: $parent")))
      case Some(parentDir:TestIODirectoryNode) ⇒ parentDir.children get child.name match {
        case None ⇒ (
          Some(parentDir.copy(children = parentDir.children + (child.name → TestIOPlainFileNode(Vector())))), IOSuccess
        )
        case Some(_) ⇒ (None, TargetFileAlreadyExists)
      }
      case Some(_) ⇒ (None, GenericIOError(new IOException(s"parent is not a directory: $parent")))
    }}
  }

  def getFileInfo(path:AbsolutePathSpec):TestIOV[GetFileInfoResult] = v {visitFileNodeAt(path) {
    case Some(_:TestIODirectoryNode) ⇒ (None, FileInfoResult(DirectoryFileInfo(path)))
    case Some(_:TestIOPlainFileNode) ⇒ (None, FileInfoResult(PlainFileInfo(path)))
    case None ⇒ (None, FileInfoResult(NonExistingFileInfo(path)))
  }}

  def hasLocalGitChanges(gitModulePath:AbsolutePathSpec):TestIOV[HasLocalGitChangesResult] = v {visitFileNodeAt(gitModulePath) {
    case None ⇒ (None, FileNotFound)
    case Some(dir:TestIODirectoryNode) ⇒ dir gitRemotes match {
      case None ⇒ (None, GenericIOError(new IOException(s"not a Git repository: $gitModulePath")))
      case Some(_) ⇒ (None, BooleanIOResult((dir.children.keySet - ".git") nonEmpty))
    }
    case Some(_) ⇒ (None, GenericIOError(new IOException(s"not a directory: $gitModulePath")))
  }}

  def listDirectoryContents(directoryPath:AbsolutePathSpec):TestIOV[ListDirectoryContentsResult] = v {visitFileNodeAt(directoryPath) {
    case None ⇒ (None, DirectoryNotFound)
    case Some(dir:TestIODirectoryNode) ⇒ (None, DirectoryListResult(dir.children.toSeq map {
      case (childName, _:TestIODirectoryNode) ⇒ DirectoryFileInfo(directoryPath/childName)
      case (childName, _:TestIOPlainFileNode) ⇒ PlainFileInfo(directoryPath/childName)
    } toSet))
    case Some(_) ⇒ (None, GenericIOError(new IOException(s"not a directory: $directoryPath")))
  }}

  def log(category:LogCategory, message:String, throwableOpt:Option[Throwable]):TestIOV[LogResult] = v {mutex synchronized {
    _logMessages :+= (category, message, throwableOpt)
    IOSuccess
  }}

  def moveFile(sourcePath:AbsolutePathSpec, targetPath:AbsolutePathSpec):TestIOV[MoveFileResult] = v {mutex synchronized {
    if(sourcePath == targetPath) GenericIOError(new IOException(s"source and target path are identical: $sourcePath"))
    else if(sourcePath startsWith targetPath) GenericIOError(new IOException("cannot move a file to one of its parents"))
    else if(targetPath startsWith sourcePath) GenericIOError(new IOException("cannot move a file into one of its subdirectories"))
    else targetPath match {
      case UserHomePathSpec ⇒ GenericIOError(new IOException("cannot replace user home"))
      case CompositeAbsolutePathSpec(targetParent, targetChild) ⇒
        sourcePath match {
          case UserHomePathSpec ⇒ GenericIOError(new IOException("cannot move user home directory"))
          case CompositeAbsolutePathSpec(sourceParent, sourceChild) ⇒
            val sourceFound:Either[MoveFileResult,TestIOFileNode] = visitFileNodeAt(sourcePath) {
              case None ⇒ (None, Left(FileNotFound))
              case Some(sourceNode) ⇒ (None, Right(sourceNode))
            }
            val wroteToTarget:Either[MoveFileResult,Unit] = sourceFound match {
              case Left(r) ⇒ Left(r)
              case Right(sourceNode) ⇒ visitFileNodeAt(targetParent) {
                case None ⇒ (None, Left(GenericIOError(new FileNotFoundException(s"parent directory of target path not found: $targetParent"))))
                case Some(targetParentNode:TestIODirectoryNode) ⇒ targetParentNode.children get targetChild.name match {
                  case Some(_) ⇒ (None, Left(TargetFileAlreadyExists))
                  case None ⇒ (Some(targetParentNode.copy(children = targetParentNode.children + (targetChild.name → sourceNode))), Right(()))
                }
                case Some(_) ⇒ (None, Left(GenericIOError(new IOException(s"parent of target path is not a directory: $targetParent"))))
              }
            }
            wroteToTarget match {
              case Left(r) ⇒ r
              case Right(_) ⇒ visitFileNodeAt(sourceParent) {
                case Some(sourceParentNode:TestIODirectoryNode) ⇒
                  (Some(sourceParentNode.copy(children = sourceParentNode.children - sourceChild.name)), IOSuccess)
                case _ ⇒ sys error "internal error"
              }
            }
        }
    }
  }}

  def moveToTrash(path:AbsolutePathSpec):TestIOV[MoveToTrashResult] = path match {
    case UserHomePathSpec ⇒ v {GenericIOError(new IOException("cannot move user home to trash"))}
    case CompositeAbsolutePathSpec(parent, child) ⇒ v {visitFileNodeAt(parent) {
      case Some(parentDir:TestIODirectoryNode) ⇒ parentDir.children get child.name match {
        case None ⇒ (None, FileNotFound)
        case Some(_) ⇒ (Some(parentDir.copy(children = parentDir.children - child.name)), IOSuccess)
      }
      case _ ⇒ (None, FileNotFound)
    }}
  }

  def readFileAsString(path:AbsolutePathSpec, charset:Charset):TestIOV[ReadFileAsStringResult] = v {visitFileNodeAt(path) {
    case Some(f:TestIOPlainFileNode) ⇒ (None, StringIOResult(new String(f.contents toArray, charset)))
    case Some(_) ⇒ (None, GenericIOError(new IOException(s"not a plain file: $path")))
    case None ⇒ (None, FileNotFound)
  }}

  def removeFile(path:AbsolutePathSpec):TestIOV[RemoveFileResult] = path match {
    case UserHomePathSpec ⇒ v {GenericIOError(new IOException("cannot remove user home"))}
    case CompositeAbsolutePathSpec(parent, child) ⇒ v {visitFileNodeAt(parent) {
      case Some(parentDir:TestIODirectoryNode) ⇒ parentDir.children get child.name match {
        case None ⇒ (None, FileNotFound)
        case Some(_:TestIOPlainFileNode) ⇒ (Some(parentDir.copy(children = parentDir.children - child.name)), IOSuccess)
        case Some(f:TestIODirectoryNode) ⇒
          if(f.children nonEmpty) (None, GenericIOError(new IOException("directory not empty: $path")))
          else (Some(parentDir.copy(children = parentDir.children - child.name)), IOSuccess)
      }
      case _ ⇒ (None, FileNotFound)
    }}
  }

  def removeGitModule(gitModulePath:AbsolutePathSpec):TestIOV[RemoveGitModuleResult] = v {visitFileNodeAt(gitModulePath) {
    case Some(dir:TestIODirectoryNode) ⇒ dir.gitRemotes match {
      case None ⇒ (None, GenericIOError(new IOException(s"not a Git repository: $gitModulePath")))
      case Some(_) ⇒ (Some(dir.copy(gitRemotes = None, children = dir.children - ".git")), IOSuccess)
    }
    case _ ⇒ (None, FileNotFound)
  }}

  def removeGitModuleRemote(gitModulePath:AbsolutePathSpec, remoteName:String):TestIOV[RemoveGitModuleRemoteResult] = v {visitFileNodeAt(gitModulePath) {
    case Some(dir:TestIODirectoryNode) ⇒ dir.gitRemotes match {
      case None ⇒ (None, GenericIOError(new IOException(s"not a Git repository: $gitModulePath")))
      case Some(remotes) if !(remotes exists {_._1 == remoteName}) ⇒ (None, GenericIOError(new IOException(s"Git remote not found: $remoteName")))
      case Some(remotes) ⇒ (Some(dir.copy(gitRemotes = Some(remotes.filterNot(_._1 == remoteName)))), IOSuccess)
    }
    case _ ⇒ (None, FileNotFound)
  }}

  def saveStringToFile(path:AbsolutePathSpec, contents:String, charset:Charset):TestIOV[SaveStringToFileResult] = path match {
    case UserHomePathSpec ⇒ v {GenericIOError(new IOException("cannot create user home as file"))}
    case CompositeAbsolutePathSpec(parent, child) ⇒ v {visitFileNodeAt(parent) {
      case None ⇒ (None, GenericIOError(new FileNotFoundException(s"parent directory not found: $parent")))
      case Some(parentDir:TestIODirectoryNode) ⇒ parentDir.children get child.name match {
        case None | Some(_:TestIOPlainFileNode) ⇒
          (Some(parentDir.copy(children = parentDir.children + (child.name → TestIOPlainFileNode(contents.getBytes(charset).toVector)))), IOSuccess)
        case _ ⇒ (None, GenericIOError(new IOException(s"directory exists at target: $path")))
      }
      case Some(_) ⇒ (None, GenericIOError(new IOException(s"parent is not a directory: $parent")))
    }}
  }

  private val mutex = new Object
  private var fsRoot:TestIOFileNode = TestIODirectoryNode()
  private var _logMessages:Seq[(LogCategory,String,Option[Throwable])] = Seq()
  private def v[A](f: ⇒A):TestIOV[A] = new TestIOV[A] {protected def execute() = f}
  private def visitFileNodeAt[A](location:AbsolutePathSpec)(f:Option[TestIOFileNode]⇒(Option[TestIOFileNode],A)):A = mutex synchronized {
    location match {
      case UserHomePathSpec ⇒ val (newRoot, result) = f(Some(fsRoot)); newRoot foreach {r ⇒ fsRoot = r}; result
      case CompositeAbsolutePathSpec(parent, child) ⇒ visitFileNodeAt(parent) {
        case None ⇒ f(None)
        case Some(parentDir:TestIODirectoryNode) ⇒ parentDir.children get child.name match {
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
object TestIO {
  private[io] sealed trait TestIOFileNode
  private[io] final case class TestIODirectoryNode(children:Map[String,TestIOFileNode]=Map(), gitRemotes:Option[Seq[(String,String)]]=None) extends TestIOFileNode
  private[io] final case class TestIOPlainFileNode(contents:Vector[Byte]) extends TestIOFileNode
}
