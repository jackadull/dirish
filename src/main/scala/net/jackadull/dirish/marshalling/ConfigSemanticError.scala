package net.jackadull.dirish.marshalling

trait ConfigSemanticError

object EmptyAbsolutePath extends ConfigSemanticError
object EmptyRelativePath extends ConfigSemanticError
final case class InvalidAbsolutePathBegin(firstPathElement:PathElementToken) extends ConfigSemanticError
