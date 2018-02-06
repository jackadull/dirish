package net.jackadull.dirish.model

import java.util.UUID

import net.jackadull.dirish.marshalling.ConfigSemanticError
import net.jackadull.dirish.path.{AbsolutePathSpec, RelativePathSpec}

sealed trait ConfigChangeError extends ConfigSemanticError

sealed trait BaseDirectoryAddError extends ConfigChangeError
sealed trait BaseDirectoryMoveError extends ConfigChangeError
sealed trait BaseDirectoryRemoveError extends ConfigChangeError
sealed trait GitModuleAddError extends ConfigChangeError
sealed trait GitModuleAddRemoteError extends ConfigChangeError
sealed trait GitModuleChangeFirstRemoteError extends ConfigChangeError
sealed trait GitModuleRemoveError extends ConfigChangeError
sealed trait GitModuleRemoveRemoteError extends ConfigChangeError
sealed trait ProjectAddError extends ConfigChangeError
sealed trait ProjectMoveError extends ConfigChangeError
sealed trait ProjectRemoveError extends ConfigChangeError

final case class BaseDirectoryAlreadyHasPath(id:UUID, path:AbsolutePathSpec) extends BaseDirectoryMoveError
final case class BaseDirectoryNotFoundForID(id:UUID) extends BaseDirectoryMoveError with BaseDirectoryRemoveError with ProjectAddError with ProjectMoveError
final case class BaseDirectoryStillReferencedByProject(id:UUID) extends BaseDirectoryRemoveError
final case class BaseDirectoryWithCrossingPathAlreadyExists(path:AbsolutePathSpec, alreadyExistingPath:AbsolutePathSpec) extends BaseDirectoryAddError with BaseDirectoryMoveError
final case class BaseDirectoryWithSameIDAlreadyExists(id:UUID) extends BaseDirectoryAddError
final case class BaseDirectoryWithSamePathAlreadyExists(path:AbsolutePathSpec) extends BaseDirectoryAddError
final case class CannotRemoveGitModuleFirstRemote(projectID:UUID, remoteName:String) extends GitModuleRemoveRemoteError
final case class GitModuleAlreadyHasAdditionalRemote(projectID:UUID, remote:(String,String)) extends GitModuleAddRemoteError with GitModuleChangeFirstRemoteError
final case class GitModuleAlreadyHasAdditionalRemoteNamed(projectID:UUID, remoteName:String) extends GitModuleAddRemoteError with GitModuleChangeFirstRemoteError
final case class GitModuleAlreadyHasFirstRemote(projectID:UUID, firstRemote:(String,String)) extends GitModuleAddRemoteError with GitModuleChangeFirstRemoteError
final case class GitModuleAlreadyHasFirstRemoteNamed(projectID:UUID, remoteName:String) extends GitModuleAddRemoteError
final case class GitModuleNotFoundForProjectID(id:UUID) extends GitModuleAddRemoteError with GitModuleChangeFirstRemoteError with GitModuleRemoveError with GitModuleRemoveRemoteError
final case class GitModuleRemoteNotFoundForName(projectID:UUID, remoteName:String) extends GitModuleRemoveRemoteError
final case class GitModuleStillHasAdditionalRemotes(projectID:UUID) extends GitModuleRemoveError
final case class ProjectAlreadyHasGitModule(id:UUID) extends GitModuleAddError
final case class ProjectAlreadyHasPath(projectID:UUID, baseDirectoryID:UUID, localPath:RelativePathSpec) extends ProjectMoveError
final case class ProjectNotFoundForID(id:UUID) extends GitModuleAddError with GitModuleAddRemoteError with GitModuleChangeFirstRemoteError with GitModuleRemoveError with GitModuleRemoveRemoteError with ProjectMoveError with ProjectRemoveError
final case class ProjectStillReferencedByGitModule(id:UUID) extends ProjectRemoveError
final case class ProjectWithCrossingPathAlreadyExists(projectID:UUID, baseDirectoryID:UUID, localPath:RelativePathSpec) extends ProjectAddError with ProjectMoveError
final case class ProjectWithSameIDAlreadyExists(id:UUID) extends ProjectAddError
final case class ProjectWithSamePathAlreadyExists(projectID:UUID, baseDirectoryID:UUID, localPath:RelativePathSpec) extends ProjectAddError with ProjectMoveError
