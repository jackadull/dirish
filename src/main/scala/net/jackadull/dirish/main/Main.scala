package net.jackadull.dirish.main

import net.jackadull.dirish.cli._
import net.jackadull.dirish.op.BlockingEitherStyles
import net.jackadull.dirish.op.combinator.FailWith
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
      case DirishSyncCommand(options) ⇒
        val mainOp = LockGuarded(UpdateDirectoryStructure) #>> {
          err ⇒ println(s"Error: $err"); FailWith(err)
        }
        mainOp instantiateIn BlockingEitherStyles // TODO add optional config path
    }
  }
}
