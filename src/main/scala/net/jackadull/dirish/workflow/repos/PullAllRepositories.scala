package net.jackadull.dirish.workflow.repos

import java.util.UUID

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.model.ProjectConfig
import net.jackadull.dirish.op.Op.ProxyOp
import net.jackadull.dirish.op.combinator.ResultIn
import net.jackadull.dirish.op.git.{HasLocalGitChanges, PullGitRepository}
import net.jackadull.dirish.op.log.Log
import net.jackadull.dirish.op.signals.Signal
import net.jackadull.dirish.op.{Op, OpError}
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.postfixOps

final case class PullAllRepositories(config:ProjectConfig) extends ProxyOp[Unit,OpError,MigrationStyle] {
  private val log = Log about this

  // TODO introduce parallelism
  protected def innerOp:Op[Unit,OpError,MigrationStyle] =
    ResultIn(config.projectIDs filter {id ⇒ config.projectFirstRemote(id) isDefined}) foreach forProject

  private def forProject(projectID:UUID):Op[Unit,OpError,MigrationStyle] = checkSignals(projectID) >> {
    case true ⇒ hasLocalChanges(projectID) >> {
      case false ⇒ performPull(projectID)
      case true ⇒ log info s"Skipping pull of project $projectID at ${absoluteProjectPath(projectID)} because it has local changes."
    }
    case false ⇒ log warn s"Skipping pull of project $projectID at ${absoluteProjectPath(projectID)} because not all of its flags are up."
  }

  private def checkSignals(projectID:UUID):Op[Boolean,OpError,MigrationStyle] = {
    def recurse(rest:Traversable[Signal[Boolean,OpError,MigrationStyle]]):Op[Boolean,OpError,MigrationStyle] =
      rest headOption match {
        case Some(signal) ⇒ signal >> {if(_) recurse(rest tail) else ResultIn(false)}
        case None ⇒ ResultIn(true)
      }
    recurse(config activeSignals projectID)
  }

  private def hasLocalChanges(projectID:UUID):Op[Boolean,OpError,MigrationStyle] =
    HasLocalGitChanges(absoluteProjectPath(projectID))

  private def performPull(projectID:UUID):Op[Unit,OpError,MigrationStyle] = {
    val projectPath = absoluteProjectPath(projectID)
    log.info(s"Pulling Git repository at $projectPath...") ~> PullGitRepository(projectPath)
  }

  private def absoluteProjectPath(projectID:UUID):AbsolutePathSpec =
    config.baseDirectoryPath(config.projectBaseDirectoryID(projectID).get).get / config.projectLocalPath(projectID).get
}
