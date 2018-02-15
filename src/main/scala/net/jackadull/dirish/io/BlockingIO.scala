package net.jackadull.dirish.io

import java.awt.Desktop
import java.awt.Desktop.Action.MOVE_TO_TRASH
import java.io.{File, FileInputStream, FileOutputStream, OutputStreamWriter}
import java.net.InetAddress
import java.nio.charset.Charset
import java.nio.file._
import java.util.logging.{Level, Logger}

import net.jackadull.dirish.io.LogCategory._
import net.jackadull.dirish.io.flags.{CachedIOFlag, IOFlag}
import net.jackadull.dirish.path.{AbsolutePathSpec, CompositeAbsolutePathSpec, PathElementSpec, UserHomePathSpec}
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.BranchTrackingStatus
import org.eclipse.jgit.transport.URIish

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
import scala.language.postfixOps

sealed abstract class BlockingV[+A] {
  private[io] lazy val value:A = execute()
  protected def execute():A
}

private[io] class FlagCache {
  private val mutex = new Object()
  private var cachedValues:Map[IOFlag,(GetFlagStatusResult,Long)] = Map()
  private var promises:Map[IOFlag,Promise[GetFlagStatusResult]] = Map()

  def lookup(flag:IOFlag, io:BlockingIO):BlockingV[GetFlagStatusResult] = flag match {
    case CachedIOFlag(uncached, ttl) ⇒ new BlockingV[GetFlagStatusResult] {
      protected def execute() = {
        val getResult:Either[(Promise[GetFlagStatusResult],Boolean),GetFlagStatusResult] = mutex synchronized {
          cachedValues get uncached match {
            case Some((result, resultWhen)) ⇒
              val resultAgeMillis = System.currentTimeMillis() - resultWhen
              if(ttl.toMillis > resultAgeMillis) {
                promises get uncached match {
                  case Some(promise) ⇒ Left(promise → true)
                  case None ⇒
                    val newPromise = Promise[GetFlagStatusResult]()
                    promises = promises + (uncached → newPromise)
                    Left(newPromise → false)
                }
              } else Right(result)
            case None ⇒
              promises get uncached match {
                case Some(promise) ⇒ Left(promise → true)
                case None ⇒
                  val newPromise = Promise[GetFlagStatusResult]()
                  promises = promises + (uncached → newPromise)
                  Left(newPromise → false)
              }
          }
        }
        getResult match {
          case Right(result) ⇒ result
          case Left((promised, true)) ⇒ Await.result(promised future, Duration.Inf)
          case Left((promised, false)) ⇒
            val result = uncached.check(io).value
            mutex synchronized {
              cachedValues = cachedValues + (uncached → (result, System.currentTimeMillis()))
              promises = promises - uncached
            }
            promised.success(result)
            result
        }
      }
    }
    case _ ⇒ flag.check(io)
  }
}

object BlockingIO extends BlockingIO(Option(System getProperty "user.home") get)
class BlockingIO(userHomePath:String) extends IO[BlockingV] {
  private val flagCache = new FlagCache

  def markForExecution[A](a:BlockingV[A]):BlockingV[Unit] = {a value; v({})}

  def aggregate[A,B](as:Traversable[BlockingV[A]])(z: ⇒B)(seqop:(B,A)⇒B, combop:(B,B)⇒B):BlockingV[B] =
    v {(as map {_ value}).aggregate(z)(seqop, combop)}
  def bind[A](a: ⇒A):BlockingV[A] = v{a}
  def flatMap[A,B](a:BlockingV[A])(f:A⇒BlockingV[B]):BlockingV[B] = v {f(a.value).value}
  def flatten[A](a:BlockingV[BlockingV[A]]):BlockingV[A] = v {a.value.value}
  def map[A,B](a:BlockingV[A])(f:A⇒B):BlockingV[B] = v {f(a value)}

  def addGitModuleRemote(gitModulePath:AbsolutePathSpec, remoteName:String, remoteURI:String):BlockingV[AddGitModuleRemoteResult] = vtry {
    val target = toFile(gitModulePath)
    if(!(target exists())) FileNotFound
    else {
      val git = Git.open(target)
      try {
        val command = git.remoteAdd()
        command.setName(remoteName)
        command.setUri(new URIish(remoteURI))
        command.call()
        IOSuccess
      } finally {git close()}
    }
  }

  def cloneGitModule(gitModulePath:AbsolutePathSpec, remoteName:String, remoteURI:String):BlockingV[CloneGitModuleResult] = vtry {
    val target = toFile(gitModulePath)
    if(target exists()) TargetFileAlreadyExists
    else {
      Git.cloneRepository().setURI(remoteURI).setDirectory(target).setRemote(remoteName).call().close()
      IOSuccess
    }
  }

  def createDirectory(directoryPath:AbsolutePathSpec):BlockingV[CreateDirectoryResult] = vtry {
    val file = toFile(directoryPath)
    if(file exists) if(file isDirectory) IOSuccess else NonDirectoryFileExistsAtGivenPath
    else if(file mkdirs()) IOSuccess else DirectoryNotCreated
  }

  def createLockFile(path:AbsolutePathSpec):BlockingV[CreateLockFileResult] = vtry {
    try {Files createFile (toFile(path) toPath); IOSuccess}
    catch {case _:FileAlreadyExistsException ⇒ TargetFileAlreadyExists}
  }

  def getFileInfo(path:AbsolutePathSpec):BlockingV[GetFileInfoResult] = vtry {
    val file = toFile(path)
    FileInfoResult(
      if(file exists()) if(file isDirectory) DirectoryFileInfo(path) else if(file isFile) PlainFileInfo(path)
        else OtherKindOfFileInfo(path)
      else NonExistingFileInfo(path)
    )
  }

  def getFlagStatus(flag:IOFlag):BlockingV[GetFlagStatusResult] = flagCache.lookup(flag, this)

  def hasLocalGitChanges(gitModulePath:AbsolutePathSpec):BlockingV[HasLocalGitChangesResult] = vtry {
    val file = toFile(gitModulePath)
    if(!(file exists())) FileNotFound
    else {
      val git = Git.open(file)
      try {
        val status = git.status().call()
        if(status.getUntracked.isEmpty && status.getUntrackedFolders.isEmpty && status.getUncommittedChanges.isEmpty) {
          val trackingStatus = BranchTrackingStatus.of(git getRepository, git.getRepository getBranch)
          if(trackingStatus==null) BooleanIOResult(false) else BooleanIOResult(trackingStatus.getAheadCount > 0)
        } else BooleanIOResult(true)
      } finally {git close()}
    }
  }

  def isHostReachable(host:String, timeoutMillis:Int):BlockingV[IsHostReachableResult] = vtry {
    BooleanIOResult(InetAddress.getByName(host).isReachable(timeoutMillis))
  }

  def listDirectoryContents(directoryPath:AbsolutePathSpec):BlockingV[ListDirectoryContentsResult] = vtry {
    val file = toFile(directoryPath)
    if(!(file.exists() && file.isDirectory)) DirectoryNotFound
    else DirectoryListResult(file.list().toSet map {ch:String ⇒
      val childFile = new File(file, ch)
      if(childFile isDirectory) DirectoryFileInfo(directoryPath/ch) else
      if(childFile isFile) PlainFileInfo(directoryPath/ch)
      else OtherKindOfFileInfo(directoryPath/ch)
    })
  }

  def log(category:LogCategory, message:String, throwableOpt:Option[Throwable]):BlockingV[LogResult] = v {
    val level = category match {
      case BeforeChange ⇒ Level.INFO
      case ExecutionFailure ⇒ Level.SEVERE
      case FailedChange ⇒ Level.SEVERE
      case NonCriticalExecutionFailure ⇒ Level.WARNING
      case PerformedChange ⇒ Level.INFO
      case SkippedChangeBecauseNotAllFlagsAreUp ⇒ Level.INFO
      case SkippedChangeForDownstreamChange ⇒ Level.FINE
    }
    val logger = Logger.getLogger("blockingio")
    throwableOpt match {
      case Some(throwable) ⇒ logger.log(level, message, throwable)
      case None ⇒ logger.log(level, message)
    }
    IOSuccess
  }

  def moveFile(sourcePath:AbsolutePathSpec, targetPath:AbsolutePathSpec):BlockingV[MoveFileResult] = vtry {
    val (source, target) = (toFile(sourcePath).toPath, toFile(targetPath).toPath)
    if(!(Files exists source)) FileNotFound
    else if(Files exists target) TargetFileAlreadyExists
    else {Files.move(source, target); IOSuccess}
  }

  def moveToTrash(path:AbsolutePathSpec):BlockingV[MoveToTrashResult] = vtry {
    val file = toFile(path)
    if(!(file exists())) FileNotFound
    else if((Desktop isDesktopSupported) && (Desktop.getDesktop isSupported MOVE_TO_TRASH))
      if (Desktop.getDesktop.moveToTrash(file)) IOSuccess else NotMovedToTrash
    else MoveToTrashNotSupported
  }

  def readFileAsString(path:AbsolutePathSpec, charset:Charset):BlockingV[ReadFileAsStringResult] = vtry {
    val input = new FileInputStream(toFile(path))
    try {StringIOResult(new String(input.readAllBytes(), charset))}
    finally {input close()}
  }

  def removeFile(path:AbsolutePathSpec):BlockingV[RemoveFileResult] = vtry {
    try {Files delete (toFile(path) toPath); IOSuccess}
    catch {case _:NoSuchFileException ⇒ FileNotFound}
  }

  def removeGitModule(gitModulePath:AbsolutePathSpec):BlockingV[RemoveGitModuleResult] = vtry {
    def recurse(f:File) {
      if(f isDirectory) {f.listFiles().foreach(recurse); f.delete()}
      else if(f exists()) f.delete()
    }
    val target = toFile(gitModulePath)
    val targetGit = new File(target, ".git")
    if(!(target exists())) FileNotFound
    else if(!((target exists()) && (target isDirectory))) NotAGitModule
    else {recurse(targetGit); IOSuccess}
  }

  def removeGitModuleRemote(gitModulePath:AbsolutePathSpec, remoteName:String):BlockingV[RemoveGitModuleRemoteResult] = vtry {
    val target = toFile(gitModulePath)
    if(!(target exists())) FileNotFound
    else {
      val git = Git.open(target)
      try {
        val command = git.remoteRemove()
        command.setName(remoteName)
        command.call()
        IOSuccess
      } finally {git close()}
    }
  }

  def saveStringToFile(path:AbsolutePathSpec, contents:String, charset:Charset):BlockingV[SaveStringToFileResult] = vtry {
    val writer = new OutputStreamWriter(new FileOutputStream(toFile(path)), charset)
    try {writer write contents} finally {writer close()}
    IOSuccess
  }

  protected def v[A](f: ⇒A):BlockingV[A] = new BlockingV[A] {protected def execute() = f}
  private def vtry[A>:GenericIOError](f: ⇒A):BlockingV[A] = v {try {f} catch {case t:Throwable ⇒ GenericIOError(t)}}

  private def toFile(pathSpec:AbsolutePathSpec):File = pathSpec match {
    case UserHomePathSpec ⇒ userHomeFile
    case CompositeAbsolutePathSpec(parent, PathElementSpec(childName)) ⇒ new File(toFile(parent), childName)
  }

  private val userHomeFile:File = new File(userHomePath)
}
