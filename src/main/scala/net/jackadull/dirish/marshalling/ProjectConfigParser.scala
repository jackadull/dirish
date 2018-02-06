package net.jackadull.dirish.marshalling

import java.util.UUID

import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers

object ProjectConfigParser extends RegexParsers {
  def root:Parser[ProjectConfigRootToken] = phrase(baseDirDefs) ^^ ProjectConfigRootToken

  private def blockClose = RenderProjectConfig.blockClose ^^ {_ ⇒ BlockCloseToken}
  private def blockOpen = RenderProjectConfig.blockOpen ^^ {_ ⇒ BlockOpenToken}
  private def gitRemoteName = """[^~\^\:\s\\]+""".r ^^ GitRemoteNameToken
  private def gitRemoteURI = """[^\s,}]+""".r ^^ GitRemoteURIToken
  private def gitModule = RenderProjectConfig.gitModule ^^ {_ ⇒ GitModuleToken}
  private def listSeparator = RenderProjectConfig.listSeparator ^^ {_ ⇒ ListSeparatorToken}
  private def pathElement = """[^/\s:{]+""".r ^^ PathElementToken
  private def pathDelimiter = RenderProjectConfig.pathDelimiter ^^ {_ ⇒ PathDelimiterToken}
  private def uuidSeparator = RenderProjectConfig.uuidSeparator ^^ {_ ⇒ UUIDSeparatorToken}
  private def uuid =  """[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}""".r ^^ {str ⇒ UUIDToken(UUID fromString str)}

  private def baseDirDef:Parser[BaseDirDefToken] = pathElements ~ uuidSeparator ~ uuid ~ blockOpen ~ directoryContentsList ~ blockClose ^^ {
    case path ~ _ ~ id ~ _ ~ contents ~ _ ⇒ BaseDirDefToken(path, id, contents)
  }
  private def baseDirDefs:Parser[List[BaseDirDefToken]] = rep(baseDirDef)
  private def directoryContents:Parser[DirectoryContentsToken] = projectDef | directoryDef
  private def directoryContentsList:Parser[List[DirectoryContentsToken]] = rep(directoryContents)
  private def directoryDef:Parser[DirectoryDefToken] = pathElements ~ blockOpen ~ directoryContentsList ~ blockClose ^^ {
    case path ~ _ ~ contents ~ _ ⇒ DirectoryDefToken(path, contents)
  }
  private def gitModuleDef:Parser[GitModuleDefToken] = gitModule ~ blockOpen ~ gitRemotes ~ blockClose ^^ {
    case _ ~ _ ~ remotes ~ _ ⇒ GitModuleDefToken(remotes)
  }
  private def gitRemote:Parser[GitRemoteToken] = gitRemoteName ~ gitRemoteURI ^^ {case name ~ uri ⇒ GitRemoteToken(name, uri)}
  private def gitRemotes:Parser[GitRemotesToken] = rep1sep(gitRemote, listSeparator) ^^ GitRemotesToken
  private def pathElements:Parser[PathElementsToken] = rep1sep(pathElement, pathDelimiter) ^^ PathElementsToken
  private def projectDef:Parser[ProjectDefToken] = pathElements ~ uuidSeparator ~ uuid ~ opt(blockOpen ~ gitModuleDef ~ blockClose) ^^ {
    case path ~ _ ~ id ~ gitOpt ⇒ ProjectDefToken(path, id, gitOpt map {case _ ~ git ~ _ ⇒ git})
  }
}
