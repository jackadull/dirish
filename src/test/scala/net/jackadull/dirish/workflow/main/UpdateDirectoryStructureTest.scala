package net.jackadull.dirish.workflow.main

import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.TimeUnit

import net.jackadull.dirish.io._
import net.jackadull.dirish.path.{AbsolutePathSpec, CompositeAbsolutePathSpec, UserHomePathSpec}
import net.jackadull.dirish.workflow.locking.LockGuarded
import org.scalatest.{FreeSpec, Matchers}

import scala.io.Source
import scala.language.higherKinds

class UpdateDirectoryStructureTest extends FreeSpec with Matchers {
  "creating a simple structure with just one project works" in {
    val io = new TestIO
    updateToConfig("just1project.dirish", io) should be (IOSuccess)
    postConditions(io)(
      normalPostUpdateConditions,
      directoryExists(UserHomePathSpec/"p2"/"prv"/"tool"/"dirish"),
      isGitRepository(UserHomePathSpec/"p2"/"prv"/"tool"/"dirish")
    )
  }

  "migrating a one-project structure to another parent (not base) directory works" in {
    val io = new TestIO
    updateToConfig("just1project.dirish", io) should be (IOSuccess)
    updateToConfig("just1project_anotherParent.dirish", io) should be (IOSuccess)
    postConditions(io)(
      normalPostUpdateConditions,
      directoryExists(UserHomePathSpec/"p2"/"prv"/"tool2"/"dirish"),
      isGitRepository(UserHomePathSpec/"p2"/"prv"/"tool2"/"dirish"),
      fileDoesNotExist(UserHomePathSpec/"p2"/"prv"/"tool"/"dirish") // TODO remove last element here …
    )
  }

  "a project whose flags are not up does not get cloned" in {
    val io = new TestIO
    io.setHostReachable("foo.bar.com", reachable = false)
    updateToConfig("just1project_and1withAFlag.dirish", io) should be (IOSuccess)
    postConditions(io)(
      normalPostUpdateConditions,
      directoryExists(UserHomePathSpec/"p2"/"prv"/"tool"/"dirish"),
      isGitRepository(UserHomePathSpec/"p2"/"prv"/"tool"/"dirish"),
      fileDoesNotExist(UserHomePathSpec/"p2"/"prv"/"tool"/"conditional")
    )
    io.setHostReachable("foo.bar.com", reachable = false)
    io.clearFlagCache()
    updateToConfig("just1project_and1withAFlag.dirish", io) should be (IOSuccess)
    postConditions(io)(
      isGitRepository(UserHomePathSpec/"p2"/"prv"/"tool"/"conditional")
    )
  }

  private def directoryExists[I[+_]](path:AbsolutePathSpec):(IO[I]⇒I[()⇒Unit]) = {io ⇒ io.map(io getFileInfo path) {
    case FileInfoResult(DirectoryFileInfo(_)) ⇒ () ⇒ ()
    case unexpected ⇒ () ⇒ fail(s"expected directory at $path, but found: $unexpected")
  }}
  private def importResource[I[+_]](resourcePath:String, targetPath:AbsolutePathSpec, io:IO[I]):I[IOResult] =
    targetPath match {
      case UserHomePathSpec ⇒ fail("cannot import into user home directly")
      case CompositeAbsolutePathSpec(parent, _) ⇒ io.flatMap(io.getFileInfo(parent)) {
        case FileInfoResult(_:NonExistingFileInfo) ⇒ io.flatMap(io.createDirectory(parent)) {
          case IOSuccess ⇒ io.saveStringToFile(targetPath, readResource(resourcePath), UTF_8)
          case unexpected ⇒ fail(s"unexpected: $unexpected")
        }
        case FileInfoResult(_:DirectoryFileInfo) ⇒ io.saveStringToFile(targetPath, readResource(resourcePath), UTF_8)
        case unexpected ⇒ fail(s"unexpected: $unexpected")
      }
    }
  private def executionResult[B,I[+_]](i:I[B], io:IO[I]):B = {
    val fut = new java.util.concurrent.CompletableFuture[B]()
    io.markForExecution(io.map(i)(v ⇒ fut.complete(v)))
    fut.get(5, TimeUnit.SECONDS)
  }
  private def fileDoesNotExist[I[+_]](path:AbsolutePathSpec):(IO[I]⇒I[()⇒Unit]) = {io ⇒ io.map(io getFileInfo path) {
    case FileInfoResult(NonExistingFileInfo(_)) ⇒ () ⇒ ()
    case unexpected ⇒ () ⇒ fail(s"expected no file at $path, but found: $unexpected")
  }}
  private def fileOrDirectoryExists[I[+_]](path:AbsolutePathSpec):(IO[I]⇒I[()⇒Unit]) = {io ⇒ io.map(io getFileInfo path) {
    case FileInfoResult(_:ExistingFileInfo) ⇒ () ⇒ ()
    case unexpected ⇒ () ⇒ fail(s"expected file or directory at $path, but found: $unexpected")
  }}
  private def isGitRepository[I[+_]](path:AbsolutePathSpec):(IO[I]⇒I[()⇒Unit]) = fileOrDirectoryExists(path/".git")
  private def normalPostUpdateConditions[I[+_]]:(IO[I]⇒I[()⇒Unit]) = {io ⇒ io.bind(() ⇒ postConditions(io)(
    plainFileExists(UserHomePathSpec/".dirish"/"internal_db.dirish"),
    fileDoesNotExist(UserHomePathSpec/".dirish"/"lockfile")
  ))}
  private def plainFileExists[I[+_]](path:AbsolutePathSpec):(IO[I]⇒I[()⇒Unit]) = {io ⇒ io.map(io getFileInfo path) {
    case FileInfoResult(PlainFileInfo(_)) ⇒ () ⇒ ()
    case unexpected ⇒ () ⇒ fail(s"expected plain file at $path, but found: $unexpected")
  }}
  private def postConditions[I[+_]](io:IO[I])(fs:(IO[I]⇒I[()⇒Unit])*) {
    def combo(a:()⇒Unit, b:()⇒Unit):()⇒Unit = {() ⇒ a(); b()}
    val v:I[()⇒Unit] = io.aggregate[()⇒Unit,()⇒Unit](fs map (_(io)))({() ⇒ ()})(combo, combo)
    executionResult(v, io)()
  }
  private def readResource(path:String):String = Source.fromResource(path).mkString
  private def updateToConfig[I[+_]](configFile:String, io:IO[I]):IOResult = {
    val v = io.flatMap(importResource(configFile, UserHomePathSpec/".dirish"/"config.dirish", io)) {
      case IOSuccess ⇒ LockGuarded(UpdateDirectoryStructure).instantiate(io)
      case unexpected ⇒ fail(s"error importing config: $unexpected")
    }
    executionResult(v, io)
  }
}
