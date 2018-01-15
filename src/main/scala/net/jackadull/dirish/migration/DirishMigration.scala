package net.jackadull.dirish.migration

import net.jackadull.dirish.migration.step.MigrationStep
import net.jackadull.dirish.model.DirishModel
import net.jackadull.dirish.model.environment.Environment

import scala.annotation.tailrec

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
  * been reached. */
object DirishMigration extends ((DirishModel,DirishModel)⇒Option[MigrationStep]) {
  def apply(current:DirishModel, target:DirishModel):Option[MigrationStep] = ??? // TODO

  @tailrec def continuously(current:DirishModel, target:DirishModel, environment:Environment, callback:DirishModel⇒Unit = {_ ⇒}):Option[Exception] =
    if(current == target) None
    else DirishMigration(current, target) match {
      case None ⇒ Some(new IllegalStateException("No further migration step found."))
      case Some(step) ⇒ step(current, environment) match {
        case Left((exception, newModelOpt)) ⇒ newModelOpt foreach callback; Some(exception)
        case Right(nextState) ⇒ callback(nextState); continuously(nextState, target, environment, callback)
      }
    }
}
