package net.jackadull.dirish.model

import java.util.UUID

import net.jackadull.dirish.path.{AbsolutePathSpec, RelativePathSpec}

import scala.language.postfixOps

sealed trait ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[ConfigChangeError,ProjectConfig]}

final case class BaseDirectoryAddedSpec(id:UUID, path:AbsolutePathSpec) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[BaseDirectoryAddError,ProjectConfig] = config addBaseDirectory (id, path)}

final case class BaseDirectoryMovedSpec(id:UUID, from:AbsolutePathSpec, to:AbsolutePathSpec) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[BaseDirectoryMoveError,ProjectConfig] = config moveBaseDirectory (id, to)}

final case class BaseDirectoryRemovedSpec(id:UUID, path:AbsolutePathSpec) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[BaseDirectoryRemoveError,ProjectConfig] = config removeBaseDirectory id}

final case class GitModuleAddedSpec(projectID:UUID, firstRemote:(String,String)) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[GitModuleAddError,ProjectConfig] = config addGitModule (projectID, firstRemote)}

final case class GitModuleFirstRemoteChangedSpec(projectID:UUID, newFirstRemote:(String,String)) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[GitModuleChangeFirstRemoteError,ProjectConfig] = config changeGitModuleFirstRemote (projectID, newFirstRemote)}

final case class GitModuleRemoteAddedSpec(projectID:UUID, remote:(String,String)) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[GitModuleAddRemoteError,ProjectConfig] = config addGitModuleRemote (projectID, remote)}

final case class GitModuleRemoteRemovedSpec(projectID:UUID, removedRemoteName:String) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[GitModuleRemoveRemoteError,ProjectConfig] = config removeGitModuleRemote (projectID, removedRemoteName)}

final case class GitModuleRemovedSpec(projectID:UUID) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[GitModuleRemoveError,ProjectConfig] = config removeGitModule projectID}

final case class ProjectAddedSpec(id:UUID, location:ProjectLocationSpec) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[ProjectAddError,ProjectConfig] = config addProject (id, location baseDirectoryID, location localPath)}

final case class ProjectMovedSpec(id:UUID, from:ProjectLocationSpec, to:ProjectLocationSpec) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[ProjectMoveError,ProjectConfig] = config moveProject(id, to baseDirectoryID, to localPath)}

final case class ProjectRemovedSpec(id:UUID, location:ProjectLocationSpec) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[ProjectRemoveError,ProjectConfig] = config removeProject id}

final case class ProjectLocationSpec(baseDirectoryID:UUID, localPath:RelativePathSpec)
