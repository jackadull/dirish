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
  val `for`:String = "for"
  val gitModule:String = "git"
  val host:String = "host"
  val listSeparator:String = ","
  val pathDelimiter:String = "/"
  val reachable:String = "reachable"
  val uuidSeparator:String = ":"
  val when:String = "when"
  val within:String = "within"

  private final case class Indent(symbol:String, depth:Int=0) {
    def deeper:Indent = copy(depth = depth + 1)
    override def toString = symbol * depth
  }
  private def activeWhen(token:ActiveWhenToken, indent:Indent):String =
    s"$indent$active $when $blockOpen\n${token.flags.map(f ⇒ flag(f, indent deeper)).mkString}$indent$blockClose\n"
  private def baseDirDef(baseDirDef:BaseDirDefToken, indent:Indent):String =
    s"$indent${pathElements(baseDirDef directory)}$uuidSeparator ${uuid(baseDirDef id)} $blockOpen\n${baseDirDef.contents map {directoryContents(_, indent deeper)} mkString}$indent$blockClose\n"
  private def directoryContents(contentsToken:DirectoryContentsToken, indent:Indent):String = contentsToken match {
    case t:ActiveWhenToken ⇒ activeWhen(t, indent)
    case t:DirectoryDefToken ⇒ directoryDef(t, indent)
    case t:ProjectDefToken ⇒ projectDef(t, indent)
  }
  private def directoryDef(directoryDef:DirectoryDefToken, indent:Indent):String =
    s"$indent${pathElements(directoryDef directory)} $blockOpen\n${directoryDef.contents map {directoryContents(_, indent deeper)} mkString}$indent$blockClose\n"
  private def duration(token:DurationToken):String = token.elements.map(timeWithUnit).mkString(" ")
  private def flag(token:FlagToken, indent:Indent, additions:Seq[String]=Seq()):String = token match {
    case CachedFlagToken(uncached, ttl) ⇒ flag(uncached, indent, additions :+ s"$braceOpen$cached ${`for`} ${duration(ttl)}$braceClose")
    case t:HostReachableToken ⇒ hostReachable(t, indent, additions)
  }
  private def gitModuleDef(gitModuleDef:GitModuleDefToken, indent:Indent):String =
    s"$indent$gitModule $blockOpen${gitRemotes(gitModuleDef remotesToken)}$blockClose\n"
  private def gitRemoteName(gitRemoteName:GitRemoteNameToken):String = gitRemoteName.name
  private def gitRemote(gitRemote:GitRemoteToken):String = s"${gitRemoteName(gitRemote nameToken)} ${gitRemoteURI(gitRemote uriToken)}"
  private def gitRemotes(gitRemotes:GitRemotesToken):String = gitRemotes.remoteTokens map gitRemote mkString s"$listSeparator "
  private def gitRemoteURI(gitRemoteURIToken:GitRemoteURIToken):String = gitRemoteURIToken.uri
  private def hostName(token:HostNameToken):String = token.hostName
  private def hostReachable(token:HostReachableToken, indent:Indent, additions:Seq[String]):String =
    s"$indent$host ${hostName(token hostNameToken)} $reachable $within ${duration(token within)}${additions.map(a ⇒ s" $a").mkString}\n"
  private def pathElement(pathElement:PathElementToken):String = pathElement.name
  private def pathElements(pathElements:PathElementsToken):String = pathElements.elements map pathElement mkString pathDelimiter
  private def projectConfigRoot(projectConfigRoot:ProjectConfigRootToken, indent:Indent):String =
    projectConfigRoot.baseDirs map {baseDirDef(_, indent)} mkString "\n"
  private def projectDef(projectDef:ProjectDefToken, indent:Indent):String =
    s"$indent${pathElements(projectDef path)}$uuidSeparator ${uuid(projectDef idToken)}" + (projectDef.properties match {
      case Seq() ⇒ ""
      case _ ⇒ s" $blockOpen\n${projectProperties(projectDef properties, indent deeper)}$indent$blockClose\n"
    })
  private def projectProperties(props:Traversable[ProjectPropertyToken], indent:Indent):String =
    props.map(projectProperty(_, indent)).toSeq.sorted.mkString
  private def projectProperty(prop:ProjectPropertyToken, indent:Indent):String = prop match {
    case p:ActiveWhenToken ⇒ activeWhen(p, indent)
    case p:GitModuleDefToken ⇒ gitModuleDef(p, indent)
  }
  private def timeWithUnit(token:TimeWithUnitToken):String = s"${token time}${token unit}"
  private def uuid(uuidToken:UUIDToken):String = uuidToken.uuid.toString
}
