package net.jackadull.dirish.migration

import net.jackadull.dirish.model._
import net.jackadull.dirish.op.Op
import net.jackadull.dirish.op.combinator.{CombinatorStyle, ResultIn}
import net.jackadull.dirish.op.git.GitStyle
import net.jackadull.dirish.op.io.IOStyle
import net.jackadull.dirish.op.log.LogStyle
import net.jackadull.dirish.op.network.NetworkStyle
import net.jackadull.dirish.op.settings.DirishSettingStyle
import net.jackadull.dirish.op.signals.SignalStyle

import scala.language.{higherKinds, postfixOps}

object Migration {
  def apply(stages:ConfigChangeStages, state:ProjectConfig):Op[MigrationResult,Nothing,MigrationStyle] =
    stage.GitRemotesRemovedMigrationStage(stages nextStage, ResultIn(MigrationResult(state, lastSavedState = Some(state))))

  type MigrationStyle[V[+_,+_]] =
    CombinatorStyle[V] with DirishSettingStyle[V] with GitStyle[V] with IOStyle[V] with LogStyle[V]
      with NetworkStyle[V] with SignalStyle[V]

  private[migration] def shouldNotPerformTransitively(changeInQuestion:ConfigChangeSpec, changesNotPerfomed:Traversable[ConfigChangeSpec]):Boolean =
    changesNotPerfomed exists {notPerformed ⇒ (changeInQuestion, notPerformed) match {
      case (a:BaseDirectoryAddedSpec, b:BaseDirectoryMovedSpec) if a.path == b.from ⇒ true
      case (a:BaseDirectoryAddedSpec, b:BaseDirectoryRemovedSpec) if a.path == b.path ⇒ true
      case (a:BaseDirectoryMovedSpec, b:BaseDirectoryMovedSpec) if a.to == b.from ⇒ true
      case (a:BaseDirectoryMovedSpec, b:BaseDirectoryRemovedSpec) if a.to == b.path ⇒ true
      case (a:GitRepositoryAddedSpec, b:ProjectAddedSpec) if a.projectID == b.id ⇒ true
      case (a:GitFirstRemoteChangedSpec, b:GitRemoteRemovedSpec) if a.projectID == b.projectID ⇒ true
      case (a:GitRemoteAddedSpec, b:GitRepositoryAddedSpec) if a.projectID == b.projectID ⇒ true
      case (a:GitRemoteAddedSpec, b:GitFirstRemoteChangedSpec) if a.projectID == b.projectID ⇒ true
      case (a:GitRemoteAddedSpec, b:GitRemoteAddedSpec) if a.projectID == b.projectID ⇒ true
      case (a:GitRemoteAddedSpec, b:GitRemoteRemovedSpec) if a.projectID == b.projectID ⇒ true
      case (a:ProjectActiveSignalAddedSpec, b:ProjectAddedSpec) if a.projectID == b.id ⇒ true
      case (a:ProjectAddedSpec, b:BaseDirectoryAddedSpec) if a.location.baseDirectoryID == b.id ⇒ true
      case (a:ProjectAddedSpec, b:ProjectMovedSpec) if a.location == b.from ⇒ true
      case (a:ProjectAddedSpec, b:ProjectRemovedSpec) if a.location == b.location ⇒ true
      case (a:ProjectMovedSpec, b:BaseDirectoryAddedSpec) if a.to.baseDirectoryID == b.id ⇒ true
      case (a:ProjectMovedSpec, b:ProjectMovedSpec) if a.to == b.from ⇒ true
      case (a:ProjectMovedSpec, b:ProjectRemovedSpec) if a.to == b.location ⇒ true
      case _ ⇒ false
    }}
}
