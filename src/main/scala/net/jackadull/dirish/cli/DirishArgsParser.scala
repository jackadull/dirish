package net.jackadull.dirish.cli

import net.jackadull.dirish.main.Main

import scala.language.postfixOps

sealed trait DirishCommand
object HelpCommand extends DirishCommand

sealed trait DirishMainCommand extends DirishCommand {
  def withOptions(opts:Seq[DirishCommandOption]):DirishMainCommand
}
final case class DirishPullCommand(options:Seq[DirishCommandOption]=Seq()) extends DirishMainCommand {
  def withOptions(opts:Seq[DirishCommandOption]):DirishMainCommand = copy(options = options ++ opts)
}
final case class DirishStatusCommand(options:Seq[DirishCommandOption]=Seq()) extends DirishMainCommand {
  def withOptions(opts:Seq[DirishCommandOption]):DirishMainCommand = copy(options = options ++ opts)
}
final case class DirishSyncCommand(options:Seq[DirishCommandOption]=Seq()) extends DirishMainCommand {
  def withOptions(opts:Seq[DirishCommandOption]):DirishMainCommand = copy(options = options ++ opts)
}

sealed trait DirishCommandOption
final case class ConfigPathOption(path:String) extends DirishCommandOption

object DirishArgsParser {
  import ArgsParser._

  val complete:ArgsParser[DirishCommand] = help |> ((options ~ main)((opts, cmd) ⇒ cmd.withOptions(opts)) <~ endOfArguments)

  def usage:String =
    s"""
      |usage: ${Main.executableName} [<options>] <command>
      |   or: ${Main.executableName} ( -? | -h | --help )    Print this help message and exit
      |
      |Options:
      |  -c, --config <arg>    Define the path of the configuration file. Default: ~/.dirish/config.dirish
      |
      |Commands:
      |  pull                  Pull the latest version of all project repositories that do not contain local
      |                        changes.
      |  status                Lists all projects that have a repository with local changes.
      |  sync                  Apply changes so that the actual project file structure matches the one specified
      |                        in the configuration file.
    """.stripMargin

  private def help:ArgsParser[DirishCommand] = ("-?" | "-h" | "--help") <~ endOfArguments ^^ {_ ⇒ HelpCommand}
  private def option:ArgsParser[DirishCommandOption] = (("-c" | "--config") ~> (expecting("config file path")!)) ^^ ConfigPathOption
  private def options:ArgsParser[Seq[DirishCommandOption]] = (option?) ^^ {_ toSeq}
  private def main:ArgsParser[DirishMainCommand] =
    ("pull" ^^ {_ ⇒ DirishPullCommand()}) |
    ("status" ^^ {_ ⇒ DirishStatusCommand()}) |
    ("sync" ^^ {_ ⇒ DirishSyncCommand()})
}
