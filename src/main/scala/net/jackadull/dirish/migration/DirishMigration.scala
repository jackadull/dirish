package net.jackadull.dirish.migration

import java.util.UUID

import net.jackadull.dirish.migration.step.MigrationStep
import net.jackadull.dirish.migration.step.MigrationStep._
import net.jackadull.dirish.model.DirishModel
import net.jackadull.dirish.model.environment.Environment
import net.jackadull.dirish.model.project.Project

import scala.annotation.tailrec
import scala.language.postfixOps

/** Computes the next migration step required to migrate from one [[net.jackadull.dirish.model.DirishModel]] closer to
  * another.
  *
  * In order to migrate from state a `current`, represented as a [[net.jackadull.dirish.model.DirishModel]], to another
  * state `target`, this function must be applied continuously, following this outline:
  *
  * <ol>
  * <li>Call `DirishMigration(current, target)`.</li>
  * <li>If the result is `None` and `current` and `target` are not equal, this is an error: The next step cannot be
  * computed. (This should not happen.)</li>
  * <li>Otherwise, the result is `Some(migrationStep)`. Apply `migrationStep` to the environment, which results in state
  * `current_modified`.</li>
  * <li>If `current_modified` is equal to `target`, the overall migration is finished successfully.</li>
  * <li>Otherwise, restart from the top, this time replacing `current` with `current_modified`.</li>
  * </ol>
  *
  * The method `DirishMigration.continously` implements this behaviour. If successful, it returns `None`. Otherwise,
  * some exception is returned. An optional callback will be called every time a new `DirishModel` has successfully
  * been reached.
  *
  * *Note*: It is important that the given source and target models validate, as otherwise certain necessary
  * preconditions might not hold, leading to unexpected behaviour. */
object DirishMigration extends ((DirishModel,DirishModel)⇒Option[MigrationStep]) {
  def apply(current:DirishModel, target:DirishModel):Option[MigrationStep] =
    MigrationAlgorithm(current, target) nextStep

  @tailrec def continuously(current:DirishModel, target:DirishModel, environment:Environment, callback:DirishModel⇒Unit = {_ ⇒}):Option[Exception] =
    if(current == target) None
    else DirishMigration(current, target) match {
      case None ⇒ Some(new IllegalStateException("No further migration step found."))
      case Some(step) ⇒ step(current, environment) match {
        case Left((exception, newModelOpt)) ⇒ newModelOpt foreach callback; Some(exception)
        case Right(nextState) ⇒ callback(nextState); continuously(nextState, target, environment, callback)
      }
    }

  private case class MigrationAlgorithm(current:DirishModel, target:DirishModel) {
    def nextStep:Option[MigrationStep] = projectRenamed orElse projectRemoved
    // TODO when obsoletePathsFolder got moved ...
    // TODO base directory removed or added

    private def forBoth[A](f:DirishModel⇒A):ForBoth[A] = ForBoth(f(current), f(target))

    private lazy val baseDirectoryByID = forBoth(_.baseDirectories.map(b ⇒ (b id, b)) toMap)
    private lazy val projectByID = forBoth(_.projects.map(p ⇒ (p id, p)) toMap)
    private lazy val projectDirectoryByProjectID = forBoth(_.projectDirectories.map(pd ⇒ (pd.projectID, pd)) toMap)

    private def baseDirectoryMoved:Option[MigrationStep] = baseDirectoryByID.current.toSeq.view.filter({
      case (bdID, _) ⇒ baseDirectoryByID.target.contains(bdID)
    }).filterNot({case (bdID, bd) ⇒ baseDirectoryByID.target.get(bdID).contains(bd)}).headOption map {
      case (bdID, currentBD) ⇒
        val newBD = baseDirectoryByID.target(bdID)
        ??? // TODO
    }

    private def obsoleteProjectsDirectoryMoved:Option[MigrationStep] = // TODO include
      if(current.obsoleteProjectsDirectory != target.obsoleteProjectsDirectory) ??? // TODO
      else ??? // TODO check base directory (really? cannot be stored easily in model!)

    private def projectRemoved:Option[MigrationStep] = (projectByID combine {(a,b) ⇒ a.keySet -- b.keySet} headOption) map {
      removedProjectID ⇒ projectDirectoryByProjectID.current.get(removedProjectID) match {
        case None ⇒
          LogWarn(s"No project directory configured for removed project: ${projectByID.current(removedProjectID)}") :+
            RemoveProjectFromModel(removedProjectID)
        case Some(projectDirectory) ⇒  baseDirectoryByID.current get (projectDirectory baseDirectoryID) match {
          case None ⇒
            LogWarn(s"Base directory with ID '${projectDirectory baseDirectoryID}' not found for removed project: ${projectByID.current(removedProjectID)}") :+
              RemoveProjectFromModel(removedProjectID)
          case Some(baseDirectory) ⇒ baseDirectoryByID.current get current.obsoleteProjectsDirectory.baseDirectoryID match {
            case None ⇒ ErrorMigrationStep(new IllegalStateException(s"Base directory with ID '${current.obsoleteProjectsDirectory.baseDirectoryID}' of obsolete projects directory not found."), true)
            case Some(obsoleteBase) ⇒
              val obsoletePath = obsoleteBase.path / current.obsoleteProjectsDirectory.relativePath
              val fromPath = baseDirectory.path / projectDirectory.relativePath
              val toPath = (obsoletePath / fromPath.lastElement).modLastElement(e ⇒ e + UUID.randomUUID)
              MoveDirectory(fromPath, toPath) :+ LogInfo(s"Moved removed project from '$fromPath' to '$toPath'.") :+
              RemoveEmptyRelativeDirectories(baseDirectory path, projectDirectory relativePath) :+
              RemoveProjectFromModel(removedProjectID)
          }
        }
      }
    }

    private def projectRenamed:Option[MigrationStep] = current.projects.view flatMap {
      case Project(id, name) ⇒ projectByID.target.get(id) flatMap {
        case Project(_, `name`) ⇒ None
        case Project(_, newName) ⇒ Some(ChangeProjectDisplayNameInModel(id, newName))
      }
    } headOption
  }

  private case class ForBoth[A](current:A, target:A) {
    def combine[B](f:(A,A)⇒B):B = f(current, target)
  }
}
