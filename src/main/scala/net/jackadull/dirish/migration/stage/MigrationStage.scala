package net.jackadull.dirish.migration.stage

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.MigrationResult
import net.jackadull.dirish.migration.stage.MigrationStage.{ChangeSkippingPolicy, NoSkipping, SkipMaybe}
import net.jackadull.dirish.migration.step.MigrationStep
import net.jackadull.dirish.model.{ConfigChangeSpec, ConfigChangeStage}
import net.jackadull.dirish.op.Op.ProxyOp
import net.jackadull.dirish.op.combinator.ResultIn
import net.jackadull.dirish.op.log.Log
import net.jackadull.dirish.op.{Op, OpError}
import net.jackadull.dirish.workflow.storage.SaveInternalDB

import scala.language.{higherKinds, postfixOps}

private[migration] trait MigrationStage[S<:ConfigChangeStage,C<:ConfigChangeSpec] extends ProxyOp[MigrationResult,Nothing,MigrationStyle] {
  def soFar:Op[MigrationResult,Nothing,MigrationStyle]
  def stage:S

  protected def changes:Traversable[C]
  protected def step(change:C):MigrationStep

  protected def changeSkippingPolicy:ChangeSkippingPolicy[C] = NoSkipping
  protected def executeStepsInParallel:Boolean = false
  protected def log:Log.about = Log about this
  protected def nextStage(thisResult:Op[MigrationResult,Nothing,MigrationStyle]):Op[MigrationResult,Nothing,MigrationStyle]

  protected def innerOp:Op[MigrationResult,Nothing,MigrationStyle] = soFar >> {state ⇒
    val (changesToExecute:Traversable[C], afterSkipping:Op[MigrationResult,Nothing,MigrationStyle]) = changeSkippingPolicy match {
      case NoSkipping ⇒ (changes, ResultIn(state))
      case maybe:SkipMaybe[C] ⇒
        val (toSkip, toExecute) = changes partition (maybe shouldSkip)
        val state2 = toSkip.foldLeft(state) {_ skippedBecauseOfDownstreamChange _}
        toSkip.size match {
          case 0 ⇒ (toExecute, ResultIn(state2))
          case 1 ⇒ (toExecute, ResultIn(state2) <~ log.debug(maybe skipMessageSingular (toSkip head, state2)))
          case _ ⇒ (toExecute, ResultIn(state2) <~ log.debug(maybe skipMessagePlural (toSkip, state2)))
        }
    }
    val stepsExecuted:Op[MigrationResult,Nothing,MigrationStyle] =
      if(executeStepsInParallel) MigrationStep.applyInParallel(changesToExecute map step, afterSkipping)
      else MigrationStep.applyInSequence(changesToExecute map step, afterSkipping)
    val savedState:Op[MigrationResult,Nothing,MigrationStyle] = stepsExecuted >> {state2 ⇒ MigrationStage.saveState(state2) #>>[MigrationResult,Nothing,MigrationStyle] {error ⇒
      ResultIn(state2.copy(errorOpt = Some(error)))
    }}
    val movingOn:Op[MigrationResult,Nothing,MigrationStyle] = savedState >> {state2 ⇒
      if(state2.errorOpt isEmpty) nextStage(ResultIn(state2)) else ResultIn(state2)
    }
    movingOn
  }
}
private[migration] object MigrationStage {
  sealed trait ChangeSkippingPolicy[-C<:ConfigChangeSpec]
  object NoSkipping extends ChangeSkippingPolicy[ConfigChangeSpec]
  abstract class SkipMaybe[-C<:ConfigChangeSpec] extends ChangeSkippingPolicy[C] {
    def shouldSkip(change:C):Boolean
    def skipMessagePlural(skipped:Traversable[C], state:MigrationResult):String
    def skipMessageSingular(skipped:C, state:MigrationResult):String
  }

  private[stage] def saveState(state:MigrationResult):Op[MigrationResult,OpError,MigrationStyle] = state.lastSavedState match {
    case Some(current) if current == state.state ⇒ ResultIn(state)
    case _ ⇒ SaveInternalDB(state state) ~> ResultIn(state.copy(lastSavedState = Some(state state)))
  }
}
