package net.jackadull.dirish.model

import java.util.UUID

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.op.OpError
import net.jackadull.dirish.op.signals.Signal
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

final case class GitRepositoryAddedSpec(projectID:UUID, firstRemote:(String,String)) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[GitRepositoryAddError,ProjectConfig] = config addGitRepository (projectID, firstRemote)}

final case class GitFirstRemoteChangedSpec(projectID:UUID, newFirstRemote:(String,String)) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[GitChangeFirstRemoteError,ProjectConfig] = config changeGitFirstRemote (projectID, newFirstRemote)}

final case class GitRemoteAddedSpec(projectID:UUID, remote:(String,String)) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[GitAddRemoteError,ProjectConfig] = config addGitRepositoryRemote (projectID, remote)}

final case class GitRemoteRemovedSpec(projectID:UUID, removedRemoteName:String) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[GitRemoveRemoteError,ProjectConfig] = config removeGitRemote (projectID, removedRemoteName)}

final case class GitRepositoryRemovedSpec(projectID:UUID) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[GitRepositoryRemoveError,ProjectConfig] = config removeGitRepository projectID}

final case class ProjectActiveSignalAddedSpec(projectID:UUID, signal:Signal[Boolean,OpError,MigrationStyle]) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[ProjectActiveSignalAddError,ProjectConfig] = config addProjectActiveSignal (projectID, signal)}

final case class ProjectActiveSignalRemovedSpec(projectID:UUID, signal:Signal[Boolean,OpError,MigrationStyle]) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[ProjectActiveSignalRemoveError,ProjectConfig] = config removeProjectActiveSignal (projectID, signal)}

final case class ProjectAddedSpec(id:UUID, location:ProjectLocationSpec) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[ProjectAddError,ProjectConfig] = config addProject (id, location baseDirectoryID, location localPath)}

final case class ProjectMovedSpec(id:UUID, from:ProjectLocationSpec, to:ProjectLocationSpec) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[ProjectMoveError,ProjectConfig] = config moveProject(id, to baseDirectoryID, to localPath)}

final case class ProjectRemovedSpec(id:UUID, location:ProjectLocationSpec) extends ConfigChangeSpec
{def applyTo(config:ProjectConfig):Either[ProjectRemoveError,ProjectConfig] = config removeProject id}

final case class ProjectLocationSpec(baseDirectoryID:UUID, localPath:RelativePathSpec)
