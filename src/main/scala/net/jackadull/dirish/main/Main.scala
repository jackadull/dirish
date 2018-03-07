package net.jackadull.dirish.main

import net.jackadull.dirish.cli._
import net.jackadull.dirish.op.combinator.{BlockingEitherCombinatorStyle, EitherV, FailWith}
import net.jackadull.dirish.op.git.BlockingGitStyle
import net.jackadull.dirish.op.io.BlockingIOStyle
import net.jackadull.dirish.op.log.BlockingSLF4JLogStyle
import net.jackadull.dirish.op.network.BlockingNetworkStyle
import net.jackadull.dirish.op.settings.{CLIDirishSettingStyle, DefaultDirishSettingStyle}
import net.jackadull.dirish.op.signals.BlockingSignalStyle
import net.jackadull.dirish.op.{Op, OpError, StyleProxies}
import net.jackadull.dirish.workflow.locking.LockGuarded
import net.jackadull.dirish.workflow.main.UpdateDirectoryStructure

import scala.language.postfixOps

object Main extends App {
  def executableName:String = "dirish"

  DirishArgsParser.complete(args toList) match {
    case failure:ArgsParserFailure ⇒
      System.err.println(s"Error: ${failure message}")
      System.err.println(s"Run '$executableName --help' for displaying a help message.")
      System.exit(1)
    case ArgsParserSuccess(cmd, _) ⇒ cmd match {
      case HelpCommand ⇒
        println(DirishArgsParser.usage trim)
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

  private def reportError(error:OpError) {System.err.println(s"Error: $error")}
}
