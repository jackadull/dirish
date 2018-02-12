package net.jackadull.dirish.workflow.storage

import java.nio.charset.StandardCharsets.UTF_8

import net.jackadull.dirish.io.IODSL.{IOOp, ParameterValue, ReadFileAsString}
import net.jackadull.dirish.io._
import net.jackadull.dirish.marshalling.{ProjectConfigParser, TokenToProjectConfig}
import net.jackadull.dirish.model.ProjectConfig

import scala.language.higherKinds

object LoadUserConfig extends IOOp[LoadUserConfigResult] {
  def instantiate[I[+_]](io:IO[I]):I[LoadUserConfigResult] = internal instantiate io

  private val internal:IOOp[LoadUserConfigResult] = ParameterValue(UserConfigPath) flatMap {path ⇒
    ReadFileAsString(path, UTF_8) map {
      case StringIOResult(contents) ⇒ ProjectConfigParser.parse(ProjectConfigParser.root, contents) match {
        case ProjectConfigParser.Success(configToken, _) ⇒ TokenToProjectConfig(configToken) match {
          case Right(config) ⇒ UserConfigLoaded(config)
          case Left(err) ⇒ CannotLoadUserConfig(CustomIOError(s"Semantical error in user config: $err"))
        }
        case err:ProjectConfigParser.NoSuccess ⇒ CannotLoadUserConfig(CustomIOError(s"User config parsing error: $err"))
      }
      case err:IOError ⇒ CannotLoadUserConfig(err)
    }
  }
}

sealed trait LoadUserConfigResult
final case class CannotLoadUserConfig(error:IOError) extends LoadUserConfigResult
final case class UserConfigLoaded(config:ProjectConfig) extends LoadUserConfigResult
