package net.jackadull.dirish.marshalling

import java.util.UUID

import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers

object ProjectConfigParser extends RegexParsers {
  def root:Parser[ProjectConfigRootToken] = phrase(topLevelTokens) ^^ ProjectConfigRootToken
  def directoryContentsList:Parser[List[DirectoryContentsToken]] = rep(directoryContents)

  private val timeUnitNames:Seq[String] = Seq("d", "day", "days", "h", "hour", "hours", "m", "min", "mins", "minute",
    "minutes", "s", "sec", "secs", "second", "seconds", "ms", "milli", "millis", "millisecond", "milliseconds")

  private def intNumber = """\d+""".r ^^ {_.toInt}
  private lazy val timeUnit = timeUnitNames.sortBy(- _.length).map(n ⇒ s"($n)").mkString("|").r ^^ {x ⇒ x}

  private def active = RenderProjectConfig.active ^^ {_ ⇒ ActiveToken}
  private def cached = RenderProjectConfig.cached ^^ {_ ⇒ CachedToken}
  private def can = RenderProjectConfig.can ^^ {_ ⇒ CanToken}
  private def connect = RenderProjectConfig.connect ^^ {_ ⇒ ConnectToken}
  private def blockClose = RenderProjectConfig.blockClose ^^ {_ ⇒ BlockCloseToken}
  private def blockOpen = RenderProjectConfig.blockOpen ^^ {_ ⇒ BlockOpenToken}
  private def braceClose = RenderProjectConfig.braceClose ^^ {_ ⇒ BraceCloseToken}
  private def braceOpen = RenderProjectConfig.braceOpen ^^ {_ ⇒ BraceOpenToken}
  private def `for` = RenderProjectConfig.`for` ^^ {_ ⇒ ForToken}
  private def gitRemoteName = """[^~\^\:\s\\]+""".r ^^ GitRemoteNameToken
  private def gitRemoteURI = """[^\s,}]+""".r ^^ GitRemoteURIToken
  private def gitRepository = RenderProjectConfig.gitRepository ^^ {_ ⇒ GitRepositoryToken}
  private def host = RenderProjectConfig.host ^^ {_ ⇒ HostToken}
  private def hostName = """[^\s}:]+""".r ^^ HostNameToken
  private def include = RenderProjectConfig.include ^^ {_ ⇒ IncludeToken}
  private def listSeparator = RenderProjectConfig.listSeparator ^^ {_ ⇒ ListSeparatorToken}
  private def pathElement = """[^/\s:{]+""".r ^^ PathElementToken
  private def pathDelimiter = RenderProjectConfig.pathDelimiter ^^ {_ ⇒ PathDelimiterToken}
  private def portSeparator = RenderProjectConfig.portSeparator ^^ {_ ⇒ PortSeparatorToken}
  private def reachable = RenderProjectConfig.reachable ^^ {_ ⇒ ReachableToken}
  private def to = RenderProjectConfig.to ^^ {_ ⇒ ToToken}
  private def uuidSeparator = RenderProjectConfig.uuidSeparator ^^ {_ ⇒ UUIDSeparatorToken}
  private def uuid =  """[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}""".r ^^ {str ⇒ UUIDToken(UUID fromString str)}
  private def when = RenderProjectConfig.when ^^ {_ ⇒ WhenToken}
  private def within = RenderProjectConfig.within ^^ {_ ⇒ WithinToken}

  private def activeWhen:Parser[ActiveWhenToken] = active ~ when ~ blockOpen ~ signals ~ blockClose ^^ {
    case _ ~ _ ~ _ ~ fs ~ _ ⇒ ActiveWhenToken(fs)
  }
  private def baseDirDef:Parser[BaseDirDefToken] = pathElements ~ uuidSeparator ~ uuid ~ blockOpen ~ directoryContentsList ~ blockClose ^^ {
    case path ~ _ ~ id ~ _ ~ contents ~ _ ⇒ BaseDirDefToken(path, id, contents)
  }
  private def canConnectToHost:Parser[CanConnectToHostToken] = can ~ connect ~ to ~ hostName ~ portSeparator ~ port ~ within ~ duration ^^ {
    case _ ~ _ ~ _ ~ host ~ _ ~ port ~ _ ~ duration ⇒ CanConnectToHostToken(host, port, duration)
  }
  private def directoryContents:Parser[DirectoryContentsToken] = activeWhen | projectDef | directoryDef | includeDirective
  private def directoryDef:Parser[DirectoryDefToken] = pathElements ~ blockOpen ~ directoryContentsList ~ blockClose ^^ {
    case path ~ _ ~ contents ~ _ ⇒ DirectoryDefToken(path, contents)
  }
  private def duration:Parser[DurationToken] = rep1(timeWithUnit) ^^ DurationToken
  private def gitRepositoryDef:Parser[GitRepositoryDefToken] = gitRepository ~ blockOpen ~ gitRemotes ~ blockClose ^^ {
    case _ ~ _ ~ remotes ~ _ ⇒ GitRepositoryDefToken(remotes)
  }
  private def gitRemote:Parser[GitRemoteToken] = gitRemoteName ~ gitRemoteURI ^^ {case name ~ uri ⇒ GitRemoteToken(name, uri)}
  private def gitRemotes:Parser[GitRemotesToken] = rep1sep(gitRemote, listSeparator) ^^ GitRemotesToken
  private def hostReachable:Parser[HostReachableToken] = host ~ hostName ~ reachable ~ within ~ duration ^^ {
    case _ ~ hn ~ _ ~ _ ~ d ⇒ HostReachableToken(hn, d)
  }
  private def includeDirective:Parser[IncludeDirectiveToken] = include ~ path ^^ {
    case _ ~ path ⇒ IncludeDirectiveToken(path)
  }
  private def path:Parser[PathElementsToken] = pathRelativeToHome | pathElements
  private def pathElements:Parser[PathElementsToken] = rep1sep(pathElement, pathDelimiter) ^^ PathElementsToken
  private def pathRelativeToHome:Parser[PathElementsToken] = "$HOME" ~ pathDelimiter ~ pathElements ^^ {
    case _ ~ _ ~ remPath ⇒ remPath.copy(elements = PathElementToken("$HOME") :: remPath.elements)
  }
  private def port:Parser[PortToken] = intNumber ^^ PortToken
  private def projectDef:Parser[ProjectDefToken] = pathElements ~ uuidSeparator ~ uuid ~ opt(blockOpen ~ (projectProperties >> validateProjectProperties) ~ blockClose) ^^ {
    case path ~ _ ~ id ~ propertiesOpt ⇒
      ProjectDefToken(path, id, propertiesOpt.map({case _ ~ ps ~ _ ⇒ ps}).getOrElse(List()))
  }
  private def projectProperties:Parser[List[ProjectPropertyToken]] = rep(projectProperty)
  private def projectProperty:Parser[ProjectPropertyToken] = gitRepositoryDef | activeWhen
  private def signal:Parser[SignalToken] = (hostReachable | canConnectToHost) ~ opt(signalCacheTTL) ^^ {
    case hr ~ None ⇒ hr
    case hr ~ Some(duration) ⇒ CachedSignalToken(hr, duration)
  }
  private def signalCacheTTL:Parser[DurationToken] = braceOpen ~ cached ~ `for` ~ duration ~ braceClose ^^ {
    case _ ~ _ ~ _ ~ d ~ _ ⇒ d
  }
  private def signals:Parser[List[SignalToken]] = rep1sep(signal, listSeparator)
  private def timeWithUnit:Parser[TimeWithUnitToken] = intNumber ~ timeUnit ^^ {
    case n ~ u ⇒ TimeWithUnitToken(n, u)
  }
  private def topLevelTokens:Parser[List[TopLevelToken]] = rep(baseDirDef | includeDirective)

  private def validateProjectProperties(els:List[ProjectPropertyToken]):Parser[List[ProjectPropertyToken]] =
    if(els.count(_.isInstanceOf[GitRepositoryDefToken])>1) err("More than one Git repository definition for project.")
    else success(els)
}
