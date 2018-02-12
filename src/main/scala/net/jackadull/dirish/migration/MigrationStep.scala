package net.jackadull.dirish.migration

import java.io.IOException

import net.jackadull.dirish.io.IODSL._
import net.jackadull.dirish.io.LogCategory.{BeforeChange, FailedChange, PerformedChange}
import net.jackadull.dirish.io._
import net.jackadull.dirish.migration.Migration.MigrationResult
import net.jackadull.dirish.migration.MigrationStep.PartialStageResult
import net.jackadull.dirish.model._
import net.jackadull.dirish.path.{AbsolutePathBase, AbsolutePathSpec, CompositeAbsolutePathSpec}

import scala.language.postfixOps

trait MigrationStep {
  def apply(state:MigrationResult):IOOp[PartialStageResult] =
    if(Migration.shouldNotPerformTransitively(change, state changesNotPerformed)) IOBind(PartialStageResult(skipped=Set(change)))
    else {
      val main:IOOp[InternalResult] = maybeLogBefore(state, mainOp(state))
      main flatMap {internalResult ⇒ interpretResult(internalResult) match {
        case Right(()) ⇒ maybeLogSuccess(state, internalResult, IOBind(PartialStageResult(
          successes = Set(change),
          unskippedFromUpstream = (state changesSkippedForDownstream) filter (includesSkippedUpstreamChange(_, state))
        )))
        case Left(error) ⇒ addLogFailure(state, internalResult, IOBind(PartialStageResult(failures = Set(change→error))), error)
      }}
    }

  type InternalResult
  def change:ConfigChangeSpec
  protected def logCommonSuffix(state:MigrationResult):String
  protected def logPrefixForBefore(state:MigrationResult):Option[String] = None
  protected def logPrefixForFailure(state:MigrationResult, result:InternalResult):String
  protected def logPrefixForSuccess(state:MigrationResult, result:InternalResult):Option[String] = None
  protected def mainOp(state:MigrationResult):IOOp[InternalResult]

  protected def interpretResult(result:InternalResult):Either[IOError,Unit] = result match {
    case ioError:IOError ⇒ Left(ioError)
    case _ ⇒ Right(())
  }
  protected def includesSkippedUpstreamChange(skipped:ConfigChangeSpec, state:MigrationResult):Boolean = false

  protected def addLogFailure[A](state:MigrationResult, result:InternalResult, op:IOOp[A], error:IOError):IOOp[A] =
    Log(FailedChange, s"${logPrefixForFailure(state, result)} ${logCommonSuffix(state)}${if(error.throwableOpt isDefined) "." else s": $error"}", error.throwableOpt) flatMap {_ ⇒ op}
  protected def maybeLogBefore[A](state:MigrationResult, op:IOOp[A]):IOOp[A] = logPrefixForBefore(state) match {
    case None ⇒ op
    case Some(prefix) ⇒ Log(BeforeChange, s"$prefix ${logCommonSuffix(state)}.") flatMap {_ ⇒ op}
  }
  protected def maybeLogSuccess[A](state:MigrationResult, result:InternalResult, op:IOOp[A]):IOOp[A] = logPrefixForSuccess(state, result) match {
    case None ⇒ op
    case Some(prefix) ⇒ op flatMap {a ⇒ Log(PerformedChange, s"$prefix ${logCommonSuffix(state)}.") map {_ ⇒ a}}
  }
}
object MigrationStep {
  def applyInParallel(steps:Traversable[MigrationStep], previous:IOOp[MigrationResult]):IOOp[MigrationResult] = {
    def combo(a:PartialStageResult, b:PartialStageResult):PartialStageResult = a ++ b
    previous flatMap {state ⇒ IOAggregate(steps map {_(state)}, PartialStageResult(), combo, combo).map(_ applyTo state)}
  }
  def applyInSequence(steps:Traversable[MigrationStep], previous:IOOp[MigrationResult]):IOOp[MigrationResult] =
    steps.foldLeft(previous) {(prev,step) ⇒ prev flatMap {state ⇒ step(state) map {partialResult ⇒ partialResult.applyTo(state)}}}

  final case class PartialStageResult(successes:Set[ConfigChangeSpec]=Set(), failures:Set[(ConfigChangeSpec,IOError)]=Set(), skipped:Set[ConfigChangeSpec]=Set(), unskippedFromUpstream:Set[ConfigChangeSpec]=Set()) {
    def applyTo(result:MigrationResult):MigrationResult = {
      val withUnskippedFromUpstream = unskippedFromUpstream.foldLeft(result)(_ performedImplicitlySkippedUpstreamChange _)
      val withSuccesses = successes.foldLeft(withUnskippedFromUpstream)(_ performedChange _)
      val withFailures = failures.foldLeft(withSuccesses)((b,t) ⇒ b failedChange (t _1, t _2))
      val withSkipped = skipped.foldLeft(withFailures)(_ notPerformingChangeTransitive _)
      withSkipped
    }
    def ++(that:PartialStageResult):PartialStageResult = copy(successes=this.successes++that.successes,
      failures=this.failures++that.failures, skipped=this.skipped++that.skipped,
      unskippedFromUpstream=this.unskippedFromUpstream++that.unskippedFromUpstream)
  }
}

final case class AddBaseDirectoryStep(change:BaseDirectoryAddedSpec) extends MigrationStep {
  type InternalResult = CreateDirectoryResult
  protected def logCommonSuffix(state:MigrationResult):String = s"base directory ${change id} at ${change path}"
  protected def logPrefixForFailure(state:MigrationResult, result:CreateDirectoryResult):String = "Failed to add"
  override protected def logPrefixForSuccess(state:MigrationResult, result:CreateDirectoryResult):Option[String] = Some("Added")
  protected def mainOp(state:MigrationResult):IOOp[CreateDirectoryResult] = CreateDirectory(change path)
}

final case class AddGitModuleRemoteStep(change:GitModuleRemoteAddedSpec) extends MigrationStep {
  type InternalResult = AddGitModuleRemoteResult
  protected def logCommonSuffix(state:MigrationResult):String =
    s"Git remote '${change.remote _1}' to ${state absoluteProjectPath (change projectID)}"
  protected def logPrefixForFailure(state:MigrationResult, result:AddGitModuleRemoteResult):String = "Failed to add"
  override protected def logPrefixForSuccess(state:MigrationResult, result:AddGitModuleRemoteResult):Option[String] =
    Some("Added")
  protected def mainOp(state:MigrationResult):IOOp[AddGitModuleRemoteResult] =
    AddGitModuleRemote(state absoluteProjectPath (change projectID), change.remote _1, change.remote _2)
}

final case class AddGitModuleStep(change:GitModuleAddedSpec) extends MigrationStep {
  type InternalResult = CloneGitModuleResult
  protected def logCommonSuffix(state:MigrationResult):String =
    s"project ${change projectID} into ${state absoluteProjectPath (change projectID)}"
  override protected def logPrefixForBefore(state:MigrationResult):Option[String] = Some("Cloning")
  protected def logPrefixForFailure(state:MigrationResult, result:CloneGitModuleResult):String = "Failed to clone"
  protected def mainOp(state:MigrationResult):IOOp[CloneGitModuleResult] = {
    val path = state.absoluteProjectPath(change projectID)
    val cloneOp = CloneGitModule(path, change.firstRemote _1, change.firstRemote _2)
    path match {
      case CompositeAbsolutePathSpec(parent, _) ⇒ GetFileInfo(parent) flatMap {
        case FileInfoResult(_:NonExistingFileInfo) ⇒ CreateDirectory(parent) flatMap {
          case IOSuccess ⇒ cloneOp
          case err:GenericIOError ⇒ IOBind(err)
          case NonDirectoryFileExistsAtGivenPath ⇒ IOBind(GenericIOError(new IOException(s"Cannot clone into a path below $parent, as a non-directory exists there.")))
          case DirectoryNotCreated ⇒ IOBind(GenericIOError(new IOException(s"Cannot create directory: $parent")))
        }
        case FileInfoResult(_:DirectoryFileInfo) ⇒ cloneOp
        case err:GenericIOError ⇒ IOBind(err)
        case _ ⇒ cloneOp
      }
      case _:AbsolutePathBase ⇒ cloneOp
    }
  }
}

final case class AddProjectStep(change:ProjectAddedSpec) extends MigrationStep { // TODO create directory, maybe?
  type InternalResult = IOResult
  protected def logCommonSuffix(state:MigrationResult):String = s"project ${change id} at ${state.baseDirectoryPath(change.location.baseDirectoryID)/change.location.localPath}"
  protected def logPrefixForFailure(state:MigrationResult, result:IOResult):String = "Failed to add"
  override protected def logPrefixForSuccess(state:MigrationResult, result:IOResult) = Some("Added")
  protected def mainOp(state:MigrationResult):IOOp[IOResult] = IOBind(IOSuccess)
}

final case class ChangeGitModuleFirstRemoteStep(change:GitModuleFirstRemoteChangedSpec) extends MigrationStep {
  type InternalResult = IOResult
  protected def logCommonSuffix(state:MigrationResult) = s"first remote of project ${change projectID} at ${state absoluteProjectPath (change projectID)}"
  protected def logPrefixForFailure(state:MigrationResult, result:IOResult) = "Failed to change"
  override protected def logPrefixForSuccess(state:MigrationResult, result:IOResult) = Some("Changed")
  protected def mainOp(state:MigrationResult) = IOSeq(Seq(
    RemoveGitModuleRemote(state absoluteProjectPath (change projectID), state firstRemoteNameOfProject (change projectID)),
    AddGitModuleRemote(state absoluteProjectPath (change projectID), change.newFirstRemote _1, change.newFirstRemote _2)
  ))
}

final case class MoveBaseDirectoryStep(change:BaseDirectoryMovedSpec) extends MigrationStep {
  type InternalResult = MoveFileResult
  protected def logCommonSuffix(state:MigrationResult) = s"base directory ${change id} from ${change from} to ${change to}"
  protected def logPrefixForFailure(state:MigrationResult, result:MoveFileResult) = "Failed to move"
  override protected def logPrefixForSuccess(state:MigrationResult, result:MoveFileResult) = Some("Moved")
  protected def mainOp(state:MigrationResult) = MoveFile(change from, change to)
}

// TODO remove empty 'from' parents
final case class MoveProjectStep(change:ProjectMovedSpec) extends MigrationStep {
  type InternalResult = MoveFileResult
  protected def logCommonSuffix(state:MigrationResult) = s"project ${change id} from ${state.baseDirectoryPath(change.from.baseDirectoryID)/change.from.localPath} to ${state.baseDirectoryPath(change.to.baseDirectoryID)/change.to.localPath}"
  protected def logPrefixForFailure(state:MigrationResult, result:MoveFileResult) = "Failed to move"
  override protected def logPrefixForSuccess(state:MigrationResult, result:MoveFileResult) = Some("Moved")
  protected def mainOp(state:MigrationResult) = {
    val targetDirectory = state.baseDirectoryPath(change.to.baseDirectoryID) / change.to.localPath
    val moveOp = MoveFile(state.baseDirectoryPath(change.from.baseDirectoryID)/change.from.localPath, targetDirectory)
    targetDirectory match {
      case CompositeAbsolutePathSpec(targetParent, _) ⇒ GetFileInfo(targetParent) flatMap {
        case FileInfoResult(DirectoryFileInfo(_)) ⇒ moveOp
        case FileInfoResult(NonExistingFileInfo(_)) ⇒ CreateDirectory(targetParent) flatMap {
          case IOSuccess ⇒ moveOp
          case err:IOError ⇒ IOBind(GenericIOError(new IOException(s"Cannot create parent directory of move target '$targetDirectory': $err")))
        }
        case FileInfoResult(unexpected) ⇒ IOBind(GenericIOError(new IOException(s"Expected directory or nothing at move target parent '$targetParent', but found: $unexpected")))
        case _:IOError ⇒ moveOp
      }
      case _ ⇒ moveOp
    }
  }
}

final case class RemoveBaseDirectoryStep(change:BaseDirectoryRemovedSpec) extends MigrationStep {
  type InternalResult = IOResult
  override protected def includesSkippedUpstreamChange(skipped:ConfigChangeSpec, state:MigrationResult) = skipped match {
    case GitModuleRemoteRemovedSpec(pid, _) ⇒ state.state.projectBaseDirectoryID(pid) contains change.id
    case GitModuleRemovedSpec(pid) ⇒ state.state.projectBaseDirectoryID(pid) contains change.id
    case ProjectRemovedSpec(_, ProjectLocationSpec(bdid, _)) ⇒ bdid == change.id
    case _ ⇒ false
  }
  protected def logCommonSuffix(state:MigrationResult) = s"base directory ${change id} at ${change path} to trash"
  protected def logPrefixForFailure(state:MigrationResult, result:IOResult) = "Failed to move"
  override protected def logPrefixForSuccess(state:MigrationResult, result:IOResult) = Some("Moved")
  protected def mainOp(state:MigrationResult) = GetFileInfo(change path) flatMap {
    case FileInfoResult(_:NonExistingFileInfo) ⇒ IOBind(IOSuccess)
    case FileInfoResult(_:DirectoryFileInfo) ⇒
      IOAggregate[Either[CustomIOError,AbsolutePathSpec],Either[Set[CustomIOError],Set[AbsolutePathSpec]]](
        state.projectIDsWithBaseDirectory(change id) map {projectID ⇒
          if(state doesProjectHaveGitModule projectID) HasLocalGitChanges(state absoluteProjectPath projectID) map {
            case BooleanIOResult(true) ⇒ Left(CustomIOError(s"Project $projectID at ${state absoluteProjectPath projectID} has local Git changes."))
            case BooleanIOResult(false) ⇒ Right(state absoluteProjectPath projectID)
            case error:IOError ⇒ Left(CustomIOError(s"Cannot check Git module for project $projectID at ${state absoluteProjectPath projectID} for local changes. ($error)"))
          }
          else IsDirectoryEmptyEnoughForRemoving(state absoluteProjectPath projectID) map {
            case BooleanIOResult(true) ⇒ Right(state absoluteProjectPath projectID)
            case BooleanIOResult(false) ⇒ Left(CustomIOError(s"Project $projectID at ${state absoluteProjectPath projectID} is not empty."))
            case error:IOError ⇒ Left(CustomIOError(s"Cannot read project $projectID at ${state absoluteProjectPath projectID}. ($error)"))
          }
        },
        Right(Set()),
        {(b:Either[Set[CustomIOError],Set[AbsolutePathSpec]], a:Either[CustomIOError,AbsolutePathSpec]) ⇒ (b,a) match {
          case (Right(paths), Right(path)) ⇒ Right(paths + path)
          case (Left(errors), Right(_)) ⇒ Left(errors)
          case (Right(_), Left(error)) ⇒ Left(Set(error))
          case (Left(errors), Left(error)) ⇒ Left(errors + error)
        }},
        {(b1:Either[Set[CustomIOError],Set[AbsolutePathSpec]], b2:Either[Set[CustomIOError],Set[AbsolutePathSpec]]) ⇒ (b1,b2) match {
          case (Right(paths1), Right(paths2)) ⇒ Right(paths1 ++ paths2)
          case (Left(errors), Right(_)) ⇒ Left(errors)
          case (Right(_), Left(errors)) ⇒ Left(errors)
          case (Left(errors1), Left(errors2)) ⇒ Left(errors1 ++ errors2)
        }}
      ) flatMap {
        case Left(oneErrorSet) if oneErrorSet.size == 1 ⇒ IOBind(oneErrorSet.head)
        case Left(severalErrors) ⇒ IOBind(CustomIOError(s"several causes:${(severalErrors map {err ⇒ s"\n- $err"}).toSeq.sorted mkString}"))
        case Right(validatedProjectDirectories) ⇒
          def recurse(toDo:Seq[AbsolutePathSpec]):IOOp[IOResult] = toDo match {
            case Seq() ⇒ IOBind(IOSuccess)
            case Seq(fst,rst@_*) if validatedProjectDirectories(fst) ⇒ recurse(rst)
            case Seq(fst,rst@_*) ⇒ GetFileInfo(fst) flatMap {
              case FileInfoResult(_:NonExistingFileInfo) ⇒ recurse(rst)
              case FileInfoResult(_:DirectoryFileInfo) ⇒
                if(validatedProjectDirectories exists {validated ⇒ validated startsWith fst}) ListDirectoryContents(fst) flatMap {
                  case DirectoryListResult(children) ⇒ recurse(rst ++ (children.toSeq map (_ path)))
                  case err:IOError ⇒ IOBind(err)
                }
                else IsDirectoryEmptyEnoughForRemoving(fst) flatMap {
                  case BooleanIOResult(true) ⇒ recurse(rst)
                  case BooleanIOResult(false) ⇒ IOBind(CustomIOError(s"Non-project directory found at: $fst"))
                  case err:IOError ⇒ IOBind(err)
                }
              case FileInfoResult(_) ⇒ IOBind(CustomIOError(s"Non-project file found at: $fst"))
              case err:IOError ⇒ IOBind(err)
            }
          }
          recurse(Seq(change path)) flatMap {
            case IOSuccess ⇒ MoveToTrash(change path)
            case anythingElse ⇒ IOBind(anythingElse)
          }
      }
    case FileInfoResult(_) ⇒ IOBind(CustomIOError(s"${change path} is not a directory."))
    case err:IOError ⇒ IOBind(err)
  }
}

final case class RemoveGitModuleRemoteStep(change:GitModuleRemoteRemovedSpec) extends MigrationStep {
  type InternalResult = RemoveGitModuleRemoteResult
  protected def logCommonSuffix(state:MigrationResult):String = s"Git remote '${change removedRemoteName}' from project ${change projectID} at ${state absoluteProjectPath (change projectID)}"
  protected def logPrefixForFailure(state:MigrationResult, result:RemoveGitModuleRemoteResult):String = "Failed to remove"
  override protected def logPrefixForSuccess(state:MigrationResult, result:RemoveGitModuleRemoteResult):Option[String] = Some("Removed")
  protected def mainOp(state:MigrationResult):IOOp[RemoveGitModuleRemoteResult] = RemoveGitModuleRemote(state absoluteProjectPath (change projectID), change removedRemoteName)
}

final case class RemoveGitModuleStep(change:GitModuleRemovedSpec) extends MigrationStep {
  type InternalResult = RemoveGitModuleResult
  override protected def includesSkippedUpstreamChange(skipped:ConfigChangeSpec, state:MigrationResult) = skipped match {
    case GitModuleRemoteRemovedSpec(pid, _) ⇒ pid == change.projectID
    case _ ⇒ false
  }
  protected def logCommonSuffix(state:MigrationResult) = s"Git module of project ${change projectID} at ${state absoluteProjectPath (change projectID)}"
  protected def logPrefixForFailure(state:MigrationResult, result:RemoveGitModuleResult) = "Failed to remove"
  override protected def logPrefixForSuccess(state:MigrationResult, result:RemoveGitModuleResult) = Some("Removed")
  protected def mainOp(state:MigrationResult) = RemoveGitModule(state absoluteProjectPath (change projectID))
}

final case class RemoveProjectStep(change:ProjectRemovedSpec) extends MigrationStep {
  type InternalResult = IOResult
  override protected def includesSkippedUpstreamChange(skipped:ConfigChangeSpec, state:MigrationResult) = skipped match {
    case GitModuleRemoteRemovedSpec(pid, _) ⇒ pid == change.id
    case GitModuleRemovedSpec(pid) ⇒ pid == change.id
    case _ ⇒ false
  }
  protected def logCommonSuffix(state:MigrationResult) = s"project ${change id} at ${state absoluteProjectPath (change id)} to trash"
  protected def logPrefixForFailure(state:MigrationResult, result:IOResult) = "Failed to move"
  override protected def logPrefixForSuccess(state:MigrationResult, result:IOResult) = Some("Moved")
  protected def mainOp(state:MigrationResult) =
    if(state.doesProjectHaveGitModule(change id)) HasLocalGitChanges(state absoluteProjectPath (change id)) flatMap {
      case BooleanIOResult(true) ⇒ IOBind(CustomIOError("Project has local Git changes."))
      case BooleanIOResult(false) ⇒ MoveToTrash(state absoluteProjectPath (change id))
      case anythingElse ⇒ IOBind(anythingElse)
    }
    else IsDirectoryEmptyEnoughForRemoving(state absoluteProjectPath (change id)) flatMap {
      case BooleanIOResult(true) ⇒ MoveToTrash(state absoluteProjectPath (change id))
      case BooleanIOResult(false) ⇒ IOBind(CustomIOError("Directory is not empty."))
      case anythingElse ⇒ IOBind(anythingElse)
    }
}
