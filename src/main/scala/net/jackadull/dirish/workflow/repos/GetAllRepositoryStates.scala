package net.jackadull.dirish.workflow.repos

import java.util.UUID

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.model.ProjectConfig
import net.jackadull.dirish.op.Op.ProxyOp
import net.jackadull.dirish.op.combinator.ResultIn
import net.jackadull.dirish.op.git.HasLocalGitChanges
import net.jackadull.dirish.op.{Op, OpError}
import net.jackadull.dirish.path.AbsolutePathSpec
import net.jackadull.dirish.workflow.repos.GetAllRepositoryStates.RepositoryState

import scala.language.postfixOps

final case class GetAllRepositoryStates(config:ProjectConfig) extends ProxyOp[Traversable[RepositoryState],OpError,MigrationStyle] {
  protected def innerOp:Op[Traversable[RepositoryState], OpError, MigrationStyle] =
    ResultIn(config.projectIDs filter {id ⇒ config.projectFirstRemote(id) isDefined} map forProject).
      foldLeft(Vector[RepositoryState]()) {(states:Vector[RepositoryState], state:RepositoryState) ⇒ states :+ state}

  private def forProject(projectID:UUID):Op[RepositoryState,OpError,MigrationStyle] = {
    val projectPath = absoluteProjectPath(projectID)
    HasLocalGitChanges(projectPath) >> {hasGitChanges ⇒ ResultIn(RepositoryState(projectID, projectPath, hasGitChanges))}
  }

  private def absoluteProjectPath(projectID:UUID):AbsolutePathSpec =
    config.baseDirectoryPath(config.projectBaseDirectoryID(projectID).get).get / config.projectLocalPath(projectID).get
}
object GetAllRepositoryStates {
  final case class RepositoryState(projectID:UUID, path:AbsolutePathSpec, hasLocalGitChanges:Boolean)
}
