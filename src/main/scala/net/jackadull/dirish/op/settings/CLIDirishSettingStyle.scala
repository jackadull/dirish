package net.jackadull.dirish.op.settings

import java.nio.file.{FileSystem, FileSystems, Path}

import net.jackadull.dirish.cli.{ConfigPathOption, DirishCommandOption}
import net.jackadull.dirish.op.combinator.CombinatorStyle
import net.jackadull.dirish.op.util.UsingCombinator
import net.jackadull.dirish.op.{GenericMessageError, OpError}
import net.jackadull.dirish.path.{AbsolutePathSpec, UserHomePathSpec}

import scala.collection.JavaConverters._
import scala.language.{higherKinds, postfixOps}

/** Picks up settings from the given command-line options, or else uses the given default. */
final case class CLIDirishSettingStyle[V[+_,+_]](
  combinatorStyle:CombinatorStyle[V],
  options:Traversable[DirishCommandOption],
  defaults:DirishSettingStyle[V],
  userHome:String=System getProperty "user.home",
  fs:FileSystem=FileSystems getDefault
) extends DirishSettingStyle[V] with UsingCombinator[V] {
  def getDirishSetting[A](setting:DirishSetting[A]):V[A,OpError] = setting match {
    case UserConfigPath ⇒ userConfigPath getOrElse (defaults getDirishSetting setting)
    case _ ⇒ defaults getDirishSetting setting
  }

  def validate:V[Any,OpError] = userConfigPath getOrElse v(())

  private lazy val userConfigPath:Option[V[AbsolutePathSpec,OpError]] = options collectFirst {
    case ConfigPathOption(path) ⇒ toAbsolutePath(path) match {
      case Left(error) ⇒ combinatorStyle failWith error
      case Right(pathSpec) ⇒ v(pathSpec)
    }
  }
  private val userHomePath:Path = fs getPath userHome

  private def toAbsolutePath(pathString:String):Either[OpError,AbsolutePathSpec] = {
    val path = (fs getPath pathString toAbsolutePath) normalize()
    if(path.startsWith(userHomePath)) {
      val elementsAfterUserHome = userHomePath.relativize(path).iterator().asScala map {_.toString}
      Right(elementsAfterUserHome.foldLeft[AbsolutePathSpec](UserHomePathSpec) {_ / _})
    } else Left(GenericMessageError(s"Cannot use '$pathString', because it is outside of the user home directory.")) // TODO allow other paths too
  }
}
