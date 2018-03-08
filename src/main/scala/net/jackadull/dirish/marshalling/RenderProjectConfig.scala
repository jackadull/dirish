package net.jackadull.dirish.marshalling

import scala.language.postfixOps

object RenderProjectConfig {
  def apply(root:ProjectConfigRootToken, indentation:String="  "):String = projectConfigRoot(root, Indent(indentation))

  val active:String = "active"
  val blockClose:String = "}"
  val blockOpen:String = "{"
  val braceClose:String = ")"
  val braceOpen:String = "("
  val cached:String = "cached"
  val can:String = "can"
  val connect:String = "connect"
  val `for`:String = "for"
  val gitRepository:String = "git"
  val host:String = "host"
  val include:String = "include"
  val listSeparator:String = ","
  val pathDelimiter:String = "/"
  val portSeparator:String = ":"
  val reachable:String = "reachable"
  val to:String = "to"
  val uuidSeparator:String = ":"
  val when:String = "when"
  val within:String = "within"

  private final case class Indent(symbol:String, depth:Int=0) {
    def deeper:Indent = copy(depth = depth + 1)
    override def toString = symbol * depth
  }
  private def activeWhen(token:ActiveWhenToken, indent:Indent):String =
    s"$indent$active $when $blockOpen\n${token.signals.map(f ⇒ signal(f, indent deeper)).mkString}$indent$blockClose\n"
  private def baseDirDef(baseDirDef:BaseDirDefToken, indent:Indent):String =
    s"$indent${pathElements(baseDirDef directory)}$uuidSeparator ${uuid(baseDirDef id)} $blockOpen\n${baseDirDef.contents map {directoryContents(_, indent deeper)} mkString}$indent$blockClose\n"
  private def canConnectToHost(token:CanConnectToHostToken, indent:Indent, additions:Seq[String]):String =
    s"$indent$can $connect $to ${hostName(token hostNameToken)}$portSeparator${port(token portToken)} $within ${duration(token within)}${additions.map(a ⇒ s" $a").mkString}\n"
  private def directoryContents(contentsToken:DirectoryContentsToken, indent:Indent):String = contentsToken match {
    case t:ActiveWhenToken ⇒ activeWhen(t, indent)
    case t:DirectoryDefToken ⇒ directoryDef(t, indent)
    case t:IncludeDirectiveToken ⇒ includeDirective(t, indent)
    case t:ProjectDefToken ⇒ projectDef(t, indent)
  }
  private def directoryDef(directoryDef:DirectoryDefToken, indent:Indent):String =
    s"$indent${pathElements(directoryDef directory)} $blockOpen\n${directoryDef.contents map {directoryContents(_, indent deeper)} mkString}$indent$blockClose\n"
  private def duration(token:DurationToken):String = token.elements.map(timeWithUnit).mkString(" ")
  private def gitRepositoryDef(gitRepositoryDef:GitRepositoryDefToken, indent:Indent):String =
    s"$indent$gitRepository $blockOpen${gitRemotes(gitRepositoryDef remotesToken)}$blockClose\n"
  private def gitRemoteName(gitRemoteName:GitRemoteNameToken):String = gitRemoteName.name
  private def gitRemote(gitRemote:GitRemoteToken):String = s"${gitRemoteName(gitRemote nameToken)} ${gitRemoteURI(gitRemote uriToken)}"
  private def gitRemotes(gitRemotes:GitRemotesToken):String = gitRemotes.remoteTokens map gitRemote mkString s"$listSeparator "
  private def gitRemoteURI(gitRemoteURIToken:GitRemoteURIToken):String = gitRemoteURIToken.uri
  private def hostName(token:HostNameToken):String = token.hostName
  private def hostReachable(token:HostReachableToken, indent:Indent, additions:Seq[String]):String =
    s"$indent$host ${hostName(token hostNameToken)} $reachable $within ${duration(token within)}${additions.map(a ⇒ s" $a").mkString}\n"
  private def includeDirective(token:IncludeDirectiveToken, indent:Indent):String =
    s"$indent$include ${pathElements(token path)}\n"
  private def pathElement(pathElement:PathElementToken):String = pathElement.name
  private def pathElements(pathElements:PathElementsToken):String = pathElements.elements map pathElement mkString pathDelimiter
  private def port(port:PortToken):String = port.portNumber.toString
  private def projectConfigRoot(projectConfigRoot:ProjectConfigRootToken, indent:Indent):String =
    projectConfigRoot.topLevel map {topLevel(_, indent)} mkString "\n"
  private def projectDef(projectDef:ProjectDefToken, indent:Indent):String =
    s"$indent${pathElements(projectDef path)}$uuidSeparator ${uuid(projectDef idToken)}" + (projectDef.properties match {
      case Seq() ⇒ ""
      case _ ⇒ s" $blockOpen\n${projectProperties(projectDef properties, indent deeper)}$indent$blockClose\n"
    })
  private def projectProperties(props:Traversable[ProjectPropertyToken], indent:Indent):String =
    props.map(projectProperty(_, indent)).toSeq.sorted.mkString
  private def projectProperty(prop:ProjectPropertyToken, indent:Indent):String = prop match {
    case p:ActiveWhenToken ⇒ activeWhen(p, indent)
    case p:GitRepositoryDefToken ⇒ gitRepositoryDef(p, indent)
  }
  private def signal(token:SignalToken, indent:Indent, additions:Seq[String]=Seq()):String = token match {
    case CachedSignalToken(uncached, ttl) ⇒ signal(uncached, indent, additions :+ s"$braceOpen$cached ${`for`} ${duration(ttl)}$braceClose")
    case t:CanConnectToHostToken ⇒ canConnectToHost(t, indent, additions)
    case t:HostReachableToken ⇒ hostReachable(t, indent, additions)
  }
  private def timeWithUnit(token:TimeWithUnitToken):String = s"${token time}${token unit}"
  private def topLevel(token:TopLevelToken, indent:Indent):String = token match {
    case t:IncludeDirectiveToken ⇒ includeDirective(t, indent)
    case t:BaseDirDefToken ⇒ baseDirDef(t, indent)
  }
  private def uuid(uuidToken:UUIDToken):String = uuidToken.uuid.toString
}
