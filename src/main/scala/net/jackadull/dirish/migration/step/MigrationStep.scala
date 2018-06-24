package net.jackadull.dirish.migration.step

import net.jackadull.dirish.migration.Migration.MigrationStyle
import net.jackadull.dirish.migration.step.MigrationStep.PartialStageResult
import net.jackadull.dirish.migration.{Migration, MigrationLogFormat, MigrationResult}
import net.jackadull.dirish.model.ConfigChangeSpec
import net.jackadull.dirish.op.combinator.ResultIn
import net.jackadull.dirish.op.log.Log
import net.jackadull.dirish.op.signals.Signal
import net.jackadull.dirish.op.{GenericThrowableError, Op, OpError}

import scala.language.{higherKinds, postfixOps, reflectiveCalls}

trait MigrationStep {
  type C<:ConfigChangeSpec

  def change:C
  protected def logFormat:MigrationLogFormat[C]
  protected def mainOp(state:MigrationResult):Op[Unit,OpError,MigrationStyle]

  protected def includesSkippedUpstreamChange(skipped:ConfigChangeSpec, state:MigrationResult):Boolean = false
  protected def signals(state:MigrationResult):Traversable[Signal[Boolean,OpError,MigrationStyle]] = Seq()

  def apply(state:MigrationResult):Op[PartialStageResult,OpError,MigrationStyle] =
    if(Migration shouldNotPerformTransitively (change, state changesNotPerformed)) ResultIn(PartialStageResult(skipped=Set(change)))
    else evaluateSignals(signals(state)) #>>[Boolean,OpError,MigrationStyle] {
      signalError ⇒ logError("Error during signal evaluation", signalError) ~> ResultIn(false)
    } >> {
      case false ⇒ (log info (logFormat skippedBecauseNotAllSignalsAreUp state)) ~> ResultIn(PartialStageResult(skippedBecauseNotAllSignalsAreUp=Set(change)))
      case true ⇒
        (logInfoOpt(logFormat doing state) ~> mainResult(state) <~ logInfoOpt(logFormat done state)) #>>[PartialStageResult,OpError,MigrationStyle] {error ⇒
          logError(logFormat failed state, error) ~> ResultIn(PartialStageResult(failures = Set(change → error)))
        }
    }

  protected val log:Log.about = Log about this
  protected def logError(messageBegin:String, error:OpError):Op[Unit,Nothing,MigrationStyle] = error match {
    case GenericThrowableError(msg, throwable) ⇒ log error (s"$messageBegin: $msg", throwable)
    case err ⇒ log error s"$messageBegin: $err"
  }
  protected def logInfoOpt(messageOpt:Option[String]):Op[Unit,Nothing,MigrationStyle] = messageOpt match {
    case Some(message) ⇒ log info message
    case None ⇒ ResultIn(())
  }

  private def evaluateSignals(rest:Traversable[Signal[Boolean,OpError,MigrationStyle]]):Op[Boolean,OpError,MigrationStyle] =
    rest headOption match {
      case None ⇒ ResultIn(true)
      case Some(signal) ⇒ signal >> {if(_) evaluateSignals(rest tail) else ResultIn(false)}
    }
  private def mainResult(state:MigrationResult):Op[PartialStageResult,OpError,MigrationStyle] = mainOp(state) ~> ResultIn(
    PartialStageResult(
      successes = Set(change),
      unskippedFromUpstream = (state changesSkippedForDownstream) filter (includesSkippedUpstreamChange(_, state))
    )
  )
}
object MigrationStep {
  final case class PartialStageResult(successes:Set[ConfigChangeSpec]=Set(), failures:Set[(ConfigChangeSpec,OpError)]=Set(), skipped:Set[ConfigChangeSpec]=Set(), unskippedFromUpstream:Set[ConfigChangeSpec]=Set(), skippedBecauseNotAllSignalsAreUp:Set[ConfigChangeSpec]=Set()) {
    def applyTo(result:MigrationResult):MigrationResult = {
      val withUnskippedFromUpstream = unskippedFromUpstream.foldLeft(result)(_ performedImplicitlySkippedUpstreamChange _)
      val withSuccesses = successes.foldLeft(withUnskippedFromUpstream)(_ performedChange _)
      val withFailures = failures.foldLeft(withSuccesses)((b,t) ⇒ b failedChange (t _1, t _2))
      val withSkipped = skipped.foldLeft(withFailures)(_ notPerformingChangeTransitive _)
      val withSkippedBecauseSignals = skippedBecauseNotAllSignalsAreUp.foldLeft(withSkipped)(_ skippedBecauseNotAllSignalsAreUp _)
      withSkippedBecauseSignals
    }
    def ++(that:PartialStageResult):PartialStageResult = copy(successes=this.successes++that.successes,
      failures=this.failures++that.failures, skipped=this.skipped++that.skipped,
      unskippedFromUpstream=this.unskippedFromUpstream++that.unskippedFromUpstream,
      skippedBecauseNotAllSignalsAreUp=this.skippedBecauseNotAllSignalsAreUp++that.skippedBecauseNotAllSignalsAreUp)
  }

  def applyInParallel(steps:Traversable[MigrationStep], soFar:Op[MigrationResult,Nothing,MigrationStyle]):Op[MigrationResult,Nothing,MigrationStyle] =
    applyInSequence(steps, soFar)

  def applyInSequence(steps:Traversable[MigrationStep], soFar:Op[MigrationResult,Nothing,MigrationStyle]):Op[MigrationResult,Nothing,MigrationStyle] = {
    def singleStepOp(step:MigrationStep, state:MigrationResult):Op[MigrationResult,Nothing,MigrationStyle] =
      (step(state) >>[MigrationResult,OpError,MigrationStyle] {partial ⇒ ResultIn(partial applyTo state)}) #>>[MigrationResult,Nothing,MigrationStyle] {error ⇒
        step.logError(step.logFormat.failed(state) + ".", error) ~> ResultIn(state.failedChange(step change, error))
      }
    steps.foldLeft(soFar) {(a:Op[MigrationResult,Nothing,MigrationStyle], b:MigrationStep) ⇒ a >> {st ⇒ singleStepOp(b, st)}}
  }
}
