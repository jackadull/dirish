package net.jackadull.dirish.workflow.storage

import net.jackadull.dirish.marshalling.{ConfigSemanticError, ProjectConfigParser}
import net.jackadull.dirish.op.OpError

sealed trait ConfigLoadError extends OpError

final case class ConfigLoadSemanticError(semanticError:ConfigSemanticError) extends ConfigLoadError
final case class ConfigLoadParsingError(parsingError:ProjectConfigParser.NoSuccess) extends ConfigLoadError
