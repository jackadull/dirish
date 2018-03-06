package net.jackadull.dirish.op.settings

import net.jackadull.dirish.op.OpError
import net.jackadull.dirish.op.combinator.{BlockingEitherCombinatorStyle, CombinatorStyle, EitherV}
import net.jackadull.dirish.op.util.UsingCombinator
import net.jackadull.dirish.path.UserHomePathSpec

import scala.language.higherKinds

trait DefaultDirishSettingStyle[V[+_,+_]] extends DirishSettingStyle[V] with UsingCombinator[V] {
  def getDirishSetting[A](setting:DirishSetting[A]):V[A,OpError] = setting match {
    case s@ApplicationDataDirectoryPath ⇒ v(s ident (UserHomePathSpec/".dirish"))
    case s@InternalDBFilePath ⇒ v(s ident (UserHomePathSpec/".dirish"/"internal_db.dirish"))
    case s@LockFilePath ⇒ v(s ident (UserHomePathSpec/".dirish"/"lockfile"))
    case s@UserConfigPath ⇒ v(s ident (UserHomePathSpec/".dirish"/"config.dirish"))
  }
}
object DefaultDirishSettingStyle extends DefaultDirishSettingStyle[EitherV] {
  protected def combinatorStyle:CombinatorStyle[EitherV] = BlockingEitherCombinatorStyle
}
