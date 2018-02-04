package net.jackadull.dirish.io

import java.awt.Desktop
import java.awt.Desktop.Action.MOVE_TO_TRASH
import java.io.File
import java.nio.file._
import java.util.logging.{Level, Logger}

import net.jackadull.dirish.io.LogCategory.{BeforeChange, FailedChange, PerformedChange, SkippedChangeForDownstreamChange}
import net.jackadull.dirish.path.{AbsolutePathSpec, CompositeAbsolutePathSpec, PathElementSpec, UserHomePathSpec}
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.BranchTrackingStatus
import org.eclipse.jgit.transport.URIish

import scala.language.postfixOps

sealed abstract class BlockingV[+A] {
  private[io] lazy val value:A = execute()
  protected def execute():A
}

object BlockingIO extends BlockingIO(Option(System getProperty "user.home") get)
class BlockingIO(userHomePath:String) extends IO[BlockingV] {
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

  def getFileInfo(path:AbsolutePathSpec):BlockingV[GetFileInfoResult] = vtry {
    val file = toFile(path)
    FileInfoResult(
      if(file exists()) if(file isDirectory) DirectoryFileInfo(path) else if(file isFile) PlainFileInfo(path)
        else OtherKindOfFileInfo(path)
      else NonExistingFileInfo(path)
    )
  }

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
      case FailedChange ⇒ Level.SEVERE
      case PerformedChange ⇒ Level.INFO
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

  private def v[A](f: ⇒A):BlockingV[A] = new BlockingV[A] {protected def execute() = f}
  private def vtry[A>:GenericIOError](f: ⇒A):BlockingV[A] = v {try {f} catch {case t:Throwable ⇒ GenericIOError(t)}}

  private def toFile(pathSpec:AbsolutePathSpec):File = pathSpec match {
    case UserHomePathSpec ⇒ userHomeFile
    case CompositeAbsolutePathSpec(parent, PathElementSpec(childName)) ⇒ new File(toFile(parent), childName)
  }

  private val userHomeFile:File = new File(userHomePath)
}
