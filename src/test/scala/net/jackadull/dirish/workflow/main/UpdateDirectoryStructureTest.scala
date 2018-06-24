package net.jackadull.dirish.workflow.main

import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.TimeUnit

import net.jackadull.dirish.op.combinator.{CombinatorStyle, EitherV, FailWith, ResultIn}
import net.jackadull.dirish.op.git.GitStyle
import net.jackadull.dirish.op.io._
import net.jackadull.dirish.op.log.LogStyle
import net.jackadull.dirish.op.network.NetworkStyle
import net.jackadull.dirish.op.settings.DirishSettingStyle
import net.jackadull.dirish.op.signals.SignalStyle
import net.jackadull.dirish.op.{GenericMessageError, Op, OpError, TestStyle}
import net.jackadull.dirish.path.{AbsolutePathSpec, CompositeAbsolutePathSpec, UserHomePathSpec}
import net.jackadull.dirish.workflow.locking.LockGuarded
import org.scalatest.{FreeSpec, Matchers}

import scala.io.Source
import scala.language.{higherKinds, postfixOps}

class UpdateDirectoryStructureTest extends FreeSpec with Matchers {
  type Style[V[+_,+_]] = CombinatorStyle[V] with DirishSettingStyle[V] with GitStyle[V] with IOStyle[V] with
    LogStyle[V] with NetworkStyle[V] with SignalStyle[V]

  "creating a simple structure with just one project works" in {
    val style = new TestStyle
    updateToConfig[EitherV]("just1project.dirish", style) should be ('right)
    postConditions[EitherV](style)(
      normalPostUpdateConditions,
      directoryExists(UserHomePathSpec/"p2"/"prv"/"tool"/"dirish"),
      isGitRepository(UserHomePathSpec/"p2"/"prv"/"tool"/"dirish")
    )
  }

  "migrating a one-project structure to another parent (not base) directory works" in {
    val io = new TestStyle
    updateToConfig[EitherV]("just1project.dirish", io) should be ('right)
    updateToConfig[EitherV]("just1project_anotherParent.dirish", io) should be ('right)
    postConditions[EitherV](io)(
      normalPostUpdateConditions,
      directoryExists(UserHomePathSpec/"p2"/"prv"/"tool2"/"dirish"),
      isGitRepository(UserHomePathSpec/"p2"/"prv"/"tool2"/"dirish"),
      fileDoesNotExist(UserHomePathSpec/"p2"/"prv"/"tool"/"dirish")
    )
  }

  "creating a simple structure with two projects works" in {
    val style = new TestStyle
    updateToConfig[EitherV]("simple_2projects.dirish", style) should be ('right)
    postConditions[EitherV](style)(
      normalPostUpdateConditions,
      directoryExists(UserHomePathSpec/"p2"/"prv"/"tool"/"dirish"),
      isGitRepository(UserHomePathSpec/"p2"/"prv"/"tool"/"dirish"),
      directoryExists(UserHomePathSpec/"p2"/"prv"/"tool"/"another_prj"),
      isGitRepository(UserHomePathSpec/"p2"/"prv"/"tool"/"another_prj")
    )
  }

  "a project whose flags are not up does not get cloned" in {
    val io = new TestStyle
    io.setHostReachable("foo.bar.com", reachable = false)
    updateToConfig[EitherV]("just1project_and1withAFlag.dirish", io) should be ('right)
    postConditions[EitherV](io)(
      normalPostUpdateConditions,
      directoryExists(UserHomePathSpec/"p2"/"prv"/"tool"/"dirish"),
      isGitRepository(UserHomePathSpec/"p2"/"prv"/"tool"/"dirish"),
      isNotAGitRepository(UserHomePathSpec/"p2"/"prv"/"tool"/"conditional")
    )
    io.setHostReachable("foo.bar.com", reachable = true)
    io.clearSignalCache()
    updateToConfig[EitherV]("just1project_and1withAFlag.dirish", io) should be ('right)
    postConditions[EitherV](io)(
      isGitRepository(UserHomePathSpec/"p2"/"prv"/"tool"/"conditional")
    )
  }

  private def directoryExists(path:AbsolutePathSpec):Op[Unit,OpError,Style] = FileKindInfo(path) >> {
    case IsDirectory ⇒ ResultIn.success
    case unexpected ⇒ FailWith(GenericMessageError(s"expected directory at $path, but found: $unexpected"))
  }
  private def importResource[V[+_,+_]](resourcePath:String, targetPath:AbsolutePathSpec, style:Style[V]):Op[Unit,IOError,Style] =
    targetPath match {
      case UserHomePathSpec ⇒ FailWith(GenericMessageError("cannot import into user home directly"))
      case CompositeAbsolutePathSpec(parent, _) ⇒ FileKindInfo(parent) >>[Unit,IOError,Style]  {
        case IsNonExistent ⇒ CreateDirectories(parent) ~> SaveStringToFile(targetPath, readResource(resourcePath), UTF_8)
        case IsDirectory ⇒ SaveStringToFile(targetPath, readResource(resourcePath), UTF_8)
        case unexpected ⇒ FailWith(GenericMessageError(s"unexpected: $unexpected"))
      }
    }
  private def executionResult[R,E,V[+_,+_]](op:Op[R,E,Style], style:Style[V]):Either[E,R] = {
    val fut = new java.util.concurrent.CompletableFuture[Either[E,R]]()
    val op2 = op #>> {error ⇒
      fut complete Left(error); FailWith(error)
    } >> {result ⇒
      fut complete Right(result); ResultIn(result)
    }
    op2 instantiateIn style
    fut.get(5, TimeUnit.SECONDS)
  }
  private def fileDoesNotExist(path:AbsolutePathSpec):Op[Unit,OpError,Style] = FileKindInfo(path) >> {
    case IsNonExistent ⇒ ResultIn.success
    case unexpected ⇒ FailWith(GenericMessageError(s"expected no file at $path, but found: $unexpected"))
  }
  private def fileOrDirectoryExists(path:AbsolutePathSpec):Op[Unit,OpError,Style] = FileKindInfo(path) >> {
    case IsRegularFile | IsDirectory ⇒ ResultIn.success
    case unexpected ⇒ FailWith(GenericMessageError(s"expected file or directory at $path, but found: $unexpected"))
  }
  private def isGitRepository(path:AbsolutePathSpec):Op[Unit,OpError,Style] = fileOrDirectoryExists(path/".git")
  private def isNotAGitRepository(path:AbsolutePathSpec):Op[Unit,OpError,Style] = fileDoesNotExist(path/".git")
  private def normalPostUpdateConditions:Op[Unit,OpError,Style] = ResultIn(Seq(
    plainFileExists(UserHomePathSpec/".dirish"/"internal_db.dirish"),
    fileDoesNotExist(UserHomePathSpec/".dirish"/"lockfile")
  )) foreach {(_:Any) ⇒ ResultIn.success}
  private def plainFileExists(path:AbsolutePathSpec):Op[Unit,OpError,Style] = FileKindInfo(path) >> {
    case IsRegularFile ⇒ ResultIn.success
    case unexpected ⇒ FailWith(GenericMessageError(s"expected plain file at $path, but found: $unexpected"))
  }
  private def postConditions[V[+_,+_]](style:Style[V])(fs:Op[Unit,OpError,Style]*) {
    fs.foreach(cond ⇒ executionResult(cond, style) match {
      case Left(GenericMessageError(msg)) ⇒ fail(msg)
      case Left(error) ⇒ fail(error toString)
      case Right(_) ⇒ ()
    })
  }
  private def readResource(path:String):String = Source.fromResource(path).mkString
  private def updateToConfig[V[+_,+_]](configFile:String, style:Style[V]):V[Unit,OpError] =
    (importResource(configFile, UserHomePathSpec/".dirish"/"config.dirish", style) ~>
      LockGuarded(UpdateDirectoryStructure)) instantiateIn style
}
