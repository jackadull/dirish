package net.jackadull.dirish.marshalling

import java.util.UUID

sealed trait ProjectConfigToken
sealed trait DirectoryContentsToken extends ProjectConfigToken
sealed trait FlagToken extends ProjectConfigToken
sealed trait ProjectPropertyToken extends ProjectConfigToken

object ActiveToken extends ProjectConfigToken
final case class ActiveWhenToken(flags:List[FlagToken]) extends DirectoryContentsToken with ProjectPropertyToken
final case class BaseDirDefToken(directory:PathElementsToken, id:UUIDToken, contents:List[DirectoryContentsToken]) extends ProjectConfigToken
object BlockCloseToken extends ProjectConfigToken
object BlockOpenToken extends ProjectConfigToken
object BraceCloseToken extends ProjectConfigToken
object BraceOpenToken extends ProjectConfigToken
object CachedToken extends ProjectConfigToken
final case class CachedFlagToken(uncached:FlagToken, timeToLive:DurationToken) extends FlagToken
final case class DirectoryDefToken(directory:PathElementsToken, contents:List[DirectoryContentsToken]) extends DirectoryContentsToken
final case class DurationToken(elements:Seq[TimeWithUnitToken]) extends ProjectConfigToken
object ForToken extends ProjectConfigToken
final case class GitModuleDefToken(remotesToken:GitRemotesToken) extends ProjectPropertyToken
object GitModuleToken extends ProjectConfigToken
final case class GitRemoteNameToken(name:String) extends ProjectConfigToken
final case class GitRemoteToken(nameToken:GitRemoteNameToken, uriToken:GitRemoteURIToken) extends ProjectConfigToken
final case class GitRemotesToken(remoteTokens:List[GitRemoteToken]) extends ProjectConfigToken
final case class GitRemoteURIToken(uri:String) extends ProjectConfigToken
final case class HostNameToken(hostName:String) extends ProjectConfigToken
final case class HostReachableToken(hostNameToken:HostNameToken, within:DurationToken) extends FlagToken
object HostToken extends ProjectConfigToken
object ListSeparatorToken extends ProjectConfigToken
object PathDelimiterToken extends ProjectConfigToken
final case class PathElementsToken(elements:List[PathElementToken]) extends ProjectConfigToken
final case class PathElementToken(name:String) extends ProjectConfigToken
final case class ProjectConfigRootToken(baseDirs:List[BaseDirDefToken]) extends ProjectConfigToken
final case class ProjectDefToken(path:PathElementsToken, idToken:UUIDToken, properties:Seq[ProjectPropertyToken]) extends DirectoryContentsToken
object ReachableToken extends ProjectConfigToken
final case class TimeWithUnitToken(time:Int, unit:String) extends ProjectConfigToken
object UUIDSeparatorToken extends ProjectConfigToken
final case class UUIDToken(uuid:UUID) extends ProjectConfigToken
object WhenToken extends ProjectConfigToken
object WithinToken extends ProjectConfigToken
