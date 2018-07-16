package net.jackadull.dirish.workflow.storage

import java.nio.charset.StandardCharsets.UTF_8

import net.jackadull.dirish.marshalling._
import net.jackadull.dirish.op.Op.ProxyOp
import net.jackadull.dirish.op.combinator.{FailWith, ResultIn}
import net.jackadull.dirish.op.io.ReadFileAsString
import net.jackadull.dirish.op.{GenericMessageError, Op, OpError}
import net.jackadull.dirish.path.{AbsolutePathSpec, CompositeAbsolutePathSpec, UserHomePathSpec}

import scala.language.postfixOps

final case class ResolveConfigIncludes(root:ProjectConfigRootToken, configPath:AbsolutePathSpec)
extends ProxyOp[ProjectConfigRootToken,OpError,StorageStyle] {
  protected def innerOp:Op[ProjectConfigRootToken,OpError,StorageStyle] = replaceRootAll(root, configPath, Set())

  private def replaceRootAll(r:ProjectConfigRootToken, loadedFrom:AbsolutePathSpec, pathsLoaded:Set[AbsolutePathSpec]):Op[ProjectConfigRootToken,OpError,StorageStyle] =
    replaceRootSingle(r, loadedFrom, pathsLoaded) match {
      case None ⇒ ResultIn(r)
      case Some(changeOp) ⇒ changeOp >> {changed ⇒ replaceRootAll(changed, loadedFrom, pathsLoaded)}
    }

  private def replaceRootSingle(r:ProjectConfigRootToken, loadedFrom:AbsolutePathSpec, pathsLoaded:Set[AbsolutePathSpec]):Option[Op[ProjectConfigRootToken,OpError,StorageStyle]] = {
    for(index ← r.topLevel.indices; tl=r.topLevel(index)) tl match {
      case IncludeDirectiveToken(includePath) ⇒
        loadRootContent(resolveIncludedPath(includePath, loadedFrom), loadedFrom, pathsLoaded + loadedFrom) >> {loadedRoot ⇒
          ResultIn(ProjectConfigRootToken(r.topLevel ++ loadedRoot.topLevel))
        }
      case bd:BaseDirDefToken ⇒ replaceDirectoryContentsListSingle(bd.contents, loadedFrom, pathsLoaded) match {
        case Some(childReplaceOp) ⇒
          return Some(childReplaceOp >> {newChildren ⇒
            ResultIn(r.copy(topLevel = r.topLevel.updated(index, bd.copy(contents = newChildren))))
          })
        case None ⇒ ()
      }
    }
    None
  }

  private def replaceDirectoryContentsListAll(cs:List[DirectoryContentsToken], loadedFrom:AbsolutePathSpec, pathsLoaded:Set[AbsolutePathSpec]):Op[List[DirectoryContentsToken],OpError,StorageStyle] =
    replaceDirectoryContentsListSingle(cs, loadedFrom, pathsLoaded) match {
      case Some(newListOp) ⇒ newListOp >> {newList ⇒ replaceDirectoryContentsListAll(newList, loadedFrom, pathsLoaded)}
      case None ⇒ ResultIn(cs)
    }

  private def replaceDirectoryContentsListSingle(cs:List[DirectoryContentsToken], loadedFrom:AbsolutePathSpec, pathsLoaded:Set[AbsolutePathSpec]):Option[Op[List[DirectoryContentsToken],OpError,StorageStyle]] = {
    for(index ← cs.indices; c=cs(index)) c match {
      case IncludeDirectiveToken(includePath) ⇒
        return Some(loadDirectoryContents(resolveIncludedPath(includePath, loadedFrom), loadedFrom, pathsLoaded + loadedFrom) >> {loadedContents ⇒
          ResultIn(cs.take(index) ++ loadedContents ++ cs.drop(index+1))
        })
      case dd:DirectoryDefToken ⇒ replaceDirectoryContentsListSingle(dd.contents, loadedFrom, pathsLoaded) match {
        case Some(childReplaceOp) ⇒
          return Some(childReplaceOp >> {newChildren ⇒
            ResultIn(cs.updated(index, dd.copy(contents = newChildren)))
          })
        case None ⇒ ()
      }
      case _:ActiveWhenToken | _:ProjectDefToken ⇒ ()
    }
    None
  }

  private def loadRootContent(includePath:AbsolutePathSpec, includeDefinedIn:AbsolutePathSpec, alreadyLoaded:Set[AbsolutePathSpec]):Op[ProjectConfigRootToken,OpError,StorageStyle] =
    if(alreadyLoaded(includePath)) FailWith(GenericMessageError(s"Circular includes detected: $includePath"))
    else ReadFileAsString(includePath, UTF_8) >> {raw ⇒
      ProjectConfigParser parse (ProjectConfigParser root, raw) match {
        case ProjectConfigParser.Success(rootToken, _) ⇒ replaceRootAll(rootToken, includePath, alreadyLoaded + includePath)
        case err:ProjectConfigParser.NoSuccess ⇒ FailWith(ConfigLoadParsingError(err))
      }
    }

  private def loadDirectoryContents(includePath:AbsolutePathSpec, includeDefinedIn:AbsolutePathSpec, alreadyLoaded:Set[AbsolutePathSpec]):Op[List[DirectoryContentsToken],OpError,StorageStyle] =
    if(alreadyLoaded(includePath)) FailWith(GenericMessageError(s"Circular includes detected: $includePath"))
    else ReadFileAsString(includePath, UTF_8) >> {raw ⇒
      ProjectConfigParser parse (ProjectConfigParser directoryContentsList, raw) match {
        case ProjectConfigParser.Success(contents, _) ⇒
          replaceDirectoryContentsListAll(contents, includePath, alreadyLoaded + includePath)
        case err:ProjectConfigParser.NoSuccess ⇒ FailWith(ConfigLoadParsingError(err))
      }
    }


  private def resolveIncludedPath(elements:PathElementsToken, base:AbsolutePathSpec):AbsolutePathSpec = elements match {
    case PathElementsToken(List(PathElementToken("$HOME"), rest@_*)) ⇒
      resolveIncludedPath(elements.copy(elements = rest toList), UserHomePathSpec)
    case _ ⇒ base match {
      case CompositeAbsolutePathSpec(parent, _) ⇒ elements.elements.foldLeft(parent) {_ / _.name}
      case _ ⇒ elements.elements.foldLeft(base) {_ / _.name}
    }
  }
}
