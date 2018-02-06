package net.jackadull.dirish.marshalling

import scala.language.postfixOps

object RenderProjectConfig {
  def apply(root:ProjectConfigRootToken, indentation:String="  "):String = projectConfigRoot(root, Indent(indentation))

  val blockClose:String = "}"
  val blockOpen:String = "{"
  val gitModule:String = "git"
  val listSeparator:String = ","
  val pathDelimiter:String = "/"
  val uuidSeparator:String = ":"

  private final case class Indent(symbol:String, depth:Int=0) {
    def deeper:Indent = copy(depth = depth + 1)
    override def toString = symbol * depth
  }
  private def baseDirDef(baseDirDef:BaseDirDefToken, indent:Indent):String =
    s"$indent${pathElements(baseDirDef directory)}$uuidSeparator ${uuid(baseDirDef id)} $blockOpen\n${baseDirDef.contents map {directoryContents(_, indent deeper)} mkString}$indent$blockClose\n"
  private def directoryContents(contentsToken:DirectoryContentsToken, indent:Indent):String = contentsToken match {
    case t:DirectoryDefToken ⇒ directoryDef(t, indent)
    case t:ProjectDefToken ⇒ projectDef(t, indent)
  }
  private def directoryDef(directoryDef:DirectoryDefToken, indent:Indent):String =
    s"$indent${pathElements(directoryDef directory)} $blockOpen\n${directoryDef.contents map {directoryContents(_, indent deeper)} mkString}$indent$blockClose\n"
  private def gitModuleDef(gitModuleDef:GitModuleDefToken, indent:Indent):String =
    s"$indent$gitModule $blockOpen${gitRemotes(gitModuleDef remotesToken)}$blockClose\n"
  private def gitRemoteName(gitRemoteName:GitRemoteNameToken):String = gitRemoteName.name
  private def gitRemote(gitRemote:GitRemoteToken):String = s"${gitRemoteName(gitRemote nameToken)} ${gitRemoteURI(gitRemote uriToken)}"
  private def gitRemotes(gitRemotes:GitRemotesToken):String = gitRemotes.remoteTokens map gitRemote mkString s"$listSeparator "
  private def gitRemoteURI(gitRemoteURIToken:GitRemoteURIToken):String = gitRemoteURIToken.uri
  private def pathElement(pathElement:PathElementToken):String = pathElement.name
  private def pathElements(pathElements:PathElementsToken):String = pathElements.elements map pathElement mkString pathDelimiter
  private def projectConfigRoot(projectConfigRoot:ProjectConfigRootToken, indent:Indent):String =
    projectConfigRoot.baseDirs map {baseDirDef(_, indent)} mkString "\n"
  private def projectDef(projectDef:ProjectDefToken, indent:Indent):String =
    s"$indent${pathElements(projectDef path)}$uuidSeparator ${uuid(projectDef idToken)}" + (projectDef.gitModuleDefOpt match {
      case None ⇒ ""
      case Some(git) ⇒ s" $blockOpen\n${gitModuleDef(git, indent deeper)}$indent$blockClose\n"
    })
  private def uuid(uuidToken:UUIDToken):String = uuidToken.uuid.toString
}
