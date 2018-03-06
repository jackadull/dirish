package net.jackadull.dirish.migration

import net.jackadull.dirish.migration.MigrationLogFormat.MLVerb
import net.jackadull.dirish.model.ConfigChangeSpec

import scala.language.postfixOps

private[migration] abstract class MigrationLogFormat[C<:ConfigChangeSpec](verb:MLVerb, change:C, logDoing:Boolean=false, logDone:Boolean=true) {
  def doing(state:MigrationResult):Option[String] = if(logDoing) Some(s"${verb doing} ${grammaticalObject(state)}.") else None
  def done(state:MigrationResult):Option[String] = if(logDone) Some(s"${verb done} ${grammaticalObject(state)}.") else None
  def failed(state:MigrationResult):String = s"${verb failed} ${grammaticalObject(state)}"
  def grammaticalObject(state:MigrationResult):String
  def skippedBecauseNotAllSignalsAreUp(state:MigrationResult):String =
    s"${verb skipped} ${grammaticalObject(state)} because not all signals are up."
}
private[migration] object MigrationLogFormat {
  final case class MLVerb(doing:String, done:String, skipped:String, failed:String)
  object MLVerb {
    val Add = MLVerb("Adding", "Added", "Skipped adding", "Failed adding")
    val Change = MLVerb("Changing", "Changed", "Skipped changing", "Failed to change")
    val Clone = MLVerb("Cloning", "Cloned", "Skipped cloning", "Failed to clone")
    val Move = MLVerb("Moving", "Moved", "Skipped moving", "Failed to move")
    val Remove = MLVerb("Removing", "Removed", "Skipped removing", "Failed to remove")
  }
}
