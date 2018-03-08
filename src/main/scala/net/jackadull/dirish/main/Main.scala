package net.jackadull.dirish.main

import net.jackadull.dirish.cli._
import net.jackadull.dirish.op.combinator.{BlockingEitherCombinatorStyle, EitherV, FailWith}
import net.jackadull.dirish.op.git.BlockingGitStyle
import net.jackadull.dirish.op.io.BlockingIOStyle
import net.jackadull.dirish.op.log.BlockingSLF4JLogStyle
import net.jackadull.dirish.op.network.BlockingNetworkStyle
import net.jackadull.dirish.op.settings.{CLIDirishSettingStyle, DefaultDirishSettingStyle}
import net.jackadull.dirish.op.signals.BlockingSignalStyle
import net.jackadull.dirish.op.{GenericThrowableError, Op, OpError, StyleProxies}
import net.jackadull.dirish.workflow.locking.LockGuarded
import net.jackadull.dirish.workflow.main.UpdateDirectoryStructure
import net.jackadull.dirish.workflow.repos.GetAllRepositoryStates.RepositoryState
import net.jackadull.dirish.workflow.repos.{GetAllRepositoryStates, PullAllRepositories}
import net.jackadull.dirish.workflow.storage.LoadInternalDB

import scala.language.{higherKinds, postfixOps}

object Main extends App {
  LogSetup()

  def executableName:String = "dirish"

  DirishArgsParser.complete(args toList) match {
    case failure:ArgsParserFailure ⇒
      System.err.println(s"Error: ${failure message}")
      System.err.println(s"Run '$executableName --help' for displaying a help message.")
      System.exit(1)
    case ArgsParserSuccess(cmd, _) ⇒ cmd match {
      case HelpCommand ⇒
        println(DirishArgsParser.usage trim)
      case DirishPullCommand(options) ⇒ runOp(LockGuarded(LoadInternalDB >> PullAllRepositories), options)
      case DirishStatusCommand(options) ⇒ runOp(LockGuarded(LoadInternalDB >> {GetAllRepositoryStates(_)} >> ReportRepositoryStates), options)
      case DirishSyncCommand(options) ⇒ runOp(LockGuarded(UpdateDirectoryStructure), options)
    }
  }

  private def runOp(op:Op[Unit,OpError,StyleProxies], options:Traversable[DirishCommandOption]) {
    style(options) match {
      case None ⇒ ()
      case Some(dirishStyle) ⇒ (op #>> {error ⇒ reportError(error); FailWith(error)}) instantiateIn dirishStyle
    }
  }

  private def style(options:Traversable[DirishCommandOption]):Option[StyleProxies[EitherV]] = {
    val combinatorStyle = BlockingEitherCombinatorStyle
    val settingStyle = CLIDirishSettingStyle(combinatorStyle, options, DefaultDirishSettingStyle)
    settingStyle validate match {
      case Right(_) ⇒ Some(StyleProxies(
        combinatorStyle = combinatorStyle,
        gitStyle = BlockingGitStyle,
        ioStyle = BlockingIOStyle,
        logStyle = BlockingSLF4JLogStyle,
        networkStyle = BlockingNetworkStyle,
        settingStyle = settingStyle,
        signalStyle = BlockingSignalStyle
      ))
      case Left(error) ⇒ reportError(error); None
    }
  }

  private def reportError(error:OpError) {error match {
    case GenericThrowableError(msg, throwable) ⇒
      System.err.println(s"Error: $msg")
      throwable.printStackTrace()
    case _ ⇒ System.err.println(s"Error: $error")
  }}

  private final case class ReportRepositoryStates(states:Traversable[RepositoryState]) extends Op[Unit,OpError,StyleProxies] {
    def instantiateIn[V[+_,+_]](style:StyleProxies[V]):V[Unit,OpError] = style resultIn {
      val pathsWithLocalChanges = states.filter(_.hasLocalGitChanges).toSeq.map(_.path.toString).sorted
      if(pathsWithLocalChanges nonEmpty) {
        println("Git repositories with local changes:")
        println()
        pathsWithLocalChanges foreach {path ⇒ println(s"\t$path")}
        println()
      }
    }
  }
}
