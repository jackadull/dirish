package net.jackadull.dirish.marshalling

import java.util.UUID

sealed trait ProjectConfigToken
sealed trait DirectoryContentsToken extends ProjectConfigToken

final case class BaseDirDefToken(directory:PathElementsToken, id:UUIDToken, contents:List[DirectoryContentsToken]) extends ProjectConfigToken
object BlockCloseToken extends ProjectConfigToken
object BlockOpenToken extends ProjectConfigToken
final case class DirectoryDefToken(directory:PathElementsToken, contents:List[DirectoryContentsToken]) extends DirectoryContentsToken
final case class GitModuleDefToken(remotesToken:GitRemotesToken) extends ProjectConfigToken
object GitModuleToken extends ProjectConfigToken
final case class GitRemoteNameToken(name:String) extends ProjectConfigToken
final case class GitRemoteToken(nameToken:GitRemoteNameToken, uriToken:GitRemoteURIToken) extends ProjectConfigToken
final case class GitRemotesToken(remoteTokens:List[GitRemoteToken]) extends ProjectConfigToken
final case class GitRemoteURIToken(uri:String) extends ProjectConfigToken
object ListSeparatorToken extends ProjectConfigToken
object PathDelimiterToken extends ProjectConfigToken
final case class PathElementsToken(elements:List[PathElementToken]) extends ProjectConfigToken
final case class PathElementToken(name:String) extends ProjectConfigToken
final case class ProjectConfigRootToken(baseDirs:List[BaseDirDefToken]) extends ProjectConfigToken
final case class ProjectDefToken(path:PathElementsToken, idToken:UUIDToken, gitModuleDefOpt:Option[GitModuleDefToken]) extends DirectoryContentsToken
object UUIDSeparatorToken extends ProjectConfigToken
final case class UUIDToken(uuid:UUID) extends ProjectConfigToken
