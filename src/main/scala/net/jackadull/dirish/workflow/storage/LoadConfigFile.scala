package net.jackadull.dirish.workflow.storage

import java.nio.charset.StandardCharsets.UTF_8

import net.jackadull.dirish.marshalling.{ProjectConfigParser, TokenToProjectConfig}
import net.jackadull.dirish.model.ProjectConfig
import net.jackadull.dirish.op.Op.ProxyOp
import net.jackadull.dirish.op.combinator.{FailWith, ResultIn}
import net.jackadull.dirish.op.io.ReadFileAsString
import net.jackadull.dirish.op.settings.DirishSetting
import net.jackadull.dirish.op.{Op, OpError}
import net.jackadull.dirish.path.AbsolutePathSpec

import scala.language.postfixOps

final case class LoadConfigFile(pathSetting:DirishSetting[AbsolutePathSpec]) extends ProxyOp[ProjectConfig,OpError,StorageStyle] {
  protected def innerOp:Op[ProjectConfig,OpError,StorageStyle] =
    pathSetting.get >> {ReadFileAsString(_, UTF_8)} >> {raw ⇒
      ProjectConfigParser parse (ProjectConfigParser root, raw) match {
        case ProjectConfigParser.Success(configToken, _) ⇒ TokenToProjectConfig(configToken) match {
          case Right(config) ⇒ ResultIn(config)
          case Left(err) ⇒ FailWith(ConfigLoadSemanticError(err))
        }
        case err:ProjectConfigParser.NoSuccess ⇒ FailWith(ConfigLoadParsingError(err))
      }
    }
}
