package net.jackadull.dirish.op.git

import java.io.{File, IOException}

import com.jcraft.jsch.JSch
import net.jackadull.dirish.op.GenericThrowableError
import net.jackadull.dirish.op.combinator.{BlockingEitherCombinatorStyle, CombinatorStyle, EitherV}
import net.jackadull.dirish.op.util.UsingCombinator
import net.jackadull.dirish.path.{AbsolutePathSpec, CompositeAbsolutePathSpec, UserHomePathSpec}
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.BranchTrackingStatus
import org.eclipse.jgit.lib.SubmoduleConfig.FetchRecurseSubmodulesMode._
import org.eclipse.jgit.transport.URIish

import scala.language.{higherKinds, postfixOps, reflectiveCalls}

class BlockingGitStyle[V[+_,+_]](protected val combinatorStyle:CombinatorStyle[V], userHome:String)
extends GitStyle[V] with UsingCombinator[V] {
  def addGitRemote(path:AbsolutePathSpec, remoteName:String, uri:String):V[Unit,GenericGitError] =
    vtry({
      val git = Git open toFile(path)
      try {
        val command = git.remoteAdd(); command setName remoteName; command setUri new URIish(uri)
        command call()
      } finally {git close()}
    }, {case t ⇒ GenericThrowableError(s"Cannot add remote '$remoteName' to $path", t)})

  def cloneGitRepository(path:AbsolutePathSpec, remoteName:String, uri:String):V[Unit,GenericGitError] =
    vtry({
      Git.cloneRepository().setURI(uri).setDirectory(toFile(path)).setCloneAllBranches(true).setCloneSubmodules(true).
        setRemote(remoteName).call().close()
    }, {case t ⇒ GenericThrowableError(s"Cannot add remote '$remoteName' to $path", t)})

  def hasLocalGitChanges(path:AbsolutePathSpec):V[Boolean,GenericGitError] =
    vtry({
      val git = Git open toFile(path)
      try {
        val status = git.status().call()
        if(status.getUntracked.isEmpty && status.getUntrackedFolders.isEmpty && status.getUncommittedChanges.isEmpty) {
          val trackingStatus = BranchTrackingStatus.of(git getRepository, git.getRepository getBranch)
          if(trackingStatus==null) false else trackingStatus.getAheadCount > 0
        } else true
      } finally {git close()}
    }, {case t ⇒ GenericThrowableError(s"Cannot check $path for local Git changes", t)})

  def pullGitRepository(path:AbsolutePathSpec):V[Unit,GenericGitError] =
    vtry({
      val git = Git.open(toFile(path))
      try {git.pull().setRecurseSubmodules(YES).call()}
      finally {git close()}
    }, {case t ⇒ GenericThrowableError(s"Cannot fetch Git repository at $path", t)})

  def removeGitRemote(path:AbsolutePathSpec, remoteName:String):V[Unit,GenericGitError] =
    vtry({
      val git = Git open toFile(path)
      try {val command = git.remoteRemove(); command.setName(remoteName); command.call()} finally {git close()}
    }, {case t ⇒ GenericThrowableError(s"Cannot remove remote '$remoteName' from $path", t)})

  def removeGitRepository(path:AbsolutePathSpec):V[Unit,GenericGitError] =
    vtry({
      def recurse(f:File) {
        if(f isDirectory) {f.listFiles().foreach(recurse); if(!f.delete()) throw new IOException(s"Failed to delete: $f")}
        else if(f exists()) if(!f.delete()) throw new IOException(s"Failed to delete: $f")
      }
      val target = new File(toFile(path), ".git")
      if(!target.isDirectory) throw new IOException(s"Not found (or not a directory): $target")
      recurse(target)
    }, {case t ⇒ GenericThrowableError(s"Cannot remove Git repository at $path", t)})


  private val userHomeFile = new File(userHome)
  private def toFile(p:AbsolutePathSpec):File = p match {
    case UserHomePathSpec ⇒ userHomeFile
    case CompositeAbsolutePathSpec(parent, child) ⇒ new File(toFile(parent), child name)
  }
}
object BlockingGitStyle extends BlockingGitStyle[EitherV](BlockingEitherCombinatorStyle, System getProperty "user.home")
