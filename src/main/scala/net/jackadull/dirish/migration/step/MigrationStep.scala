package net.jackadull.dirish.migration.step

import java.util.UUID

import net.jackadull.dirish.migration.step.MigrationStep.MigrationSteps
import net.jackadull.dirish.model.DirishModel
import net.jackadull.dirish.model.environment.Environment
import net.jackadull.dirish.model.fs.{AbsolutePathSpec, RelativePathSpec}

import scala.annotation.tailrec

/** A command that can be executed on an environment, and that changes one [[net.jackadull.dirish.model.DirishModel]],
  * returning the changed version. */
trait MigrationStep extends ((DirishModel,Environment)⇒Either[(Exception,Option[DirishModel]),DirishModel]) {
  def :+(that:MigrationStep):MigrationStep = (this, that) match {
    case (MigrationSteps(seq1), MigrationSteps(seq2)) ⇒ MigrationSteps(seq1 ++ seq2)
    case (MigrationSteps(seq1), _) ⇒ MigrationSteps(seq1 :+ that)
    case (_, MigrationSteps(seq2)) ⇒ MigrationSteps(this +: seq2)
    case (_,_) ⇒ MigrationSteps(Seq(this, that))
  }
}
object MigrationStep {
  case class ChangeProjectDisplayNameInModel(projectID:UUID, newDisplayName:String) extends MigrationStep {
    def apply(current:DirishModel, env:Environment):Either[(Exception,Option[DirishModel]),DirishModel] =
      current.projects find {_.id == projectID} match {
        case None ⇒ Left((new IllegalStateException(s"Project with ID $projectID not found."), None))
        case Some(project) if project.displayName == newDisplayName ⇒ Right(current)
        case Some(project) ⇒ Right(current modProjects {_ - project + project.copy(displayName = newDisplayName)})
      }
  }

  case class ErrorMigrationStep(exception:Exception, passThroughModel:Boolean) extends MigrationStep {
    def apply(current:DirishModel, env:Environment):Either[(Exception,Option[DirishModel]),DirishModel] =
      Left((exception, if(passThroughModel) Some(current) else None))
  }

  case class LogInfo(message:String) extends MigrationStep {
    def apply(current:DirishModel, env:Environment):Either[(Exception,Option[DirishModel]),DirishModel] =
    {env.logInfo(message); Right(current)}
  }

  case class LogWarn(message:String) extends MigrationStep {
    def apply(current:DirishModel, env:Environment):Either[(Exception,Option[DirishModel]),DirishModel] =
    {env.logWarn(message); Right(current)}
  }

  case class MigrationSteps(seq:Seq[MigrationStep]) extends MigrationStep {
    def apply(current:DirishModel, env:Environment):Either[(Exception,Option[DirishModel]),DirishModel] = {
      @tailrec def recurse(remaining:Seq[MigrationStep], model:DirishModel):Either[(Exception,Option[DirishModel]),DirishModel] = remaining match {
        case Seq() ⇒ Right(model)
        case Seq(fst, rst@_*) ⇒ fst(model, env) match {
          case Left(err) ⇒ Left(err)
          case Right(newModel) ⇒ recurse(rst, newModel)
        }
      }
      recurse(seq, current)
    }
  }

  // means that the source directory should become the target directory, and not be contained in the target directory
  case class MoveDirectory(sourceDirectory:AbsolutePathSpec, targetDirectory:AbsolutePathSpec) extends MigrationStep {
    def apply(current:DirishModel, env:Environment):Either[(Exception,Option[DirishModel]),DirishModel] = ??? // TODO
  }

  case class RemoveEmptyRelativeDirectories(base:AbsolutePathSpec, relative:RelativePathSpec) extends MigrationStep {
    def apply(current:DirishModel, env:Environment):Either[(Exception,Option[DirishModel]),DirishModel] = ??? // TODO
  }

  case class RemoveProjectFromModel(projectID:UUID) extends MigrationStep {
    def apply(current:DirishModel, env:Environment):Either[(Exception,Option[DirishModel]),DirishModel] =
      Right(current.
        modProjects(ps ⇒ ps.filterNot(p ⇒ p.id==projectID)).
        modGitModules(gms ⇒ gms.filterNot(gm ⇒ gm.projectID==projectID)).
        modProjectDirectories(pds ⇒ pds.filter(pd ⇒ pd.projectID==projectID))
      )
  }
}
