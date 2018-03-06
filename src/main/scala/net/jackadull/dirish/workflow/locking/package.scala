package net.jackadull.dirish.workflow

import net.jackadull.dirish.op.combinator.CombinatorStyle
import net.jackadull.dirish.op.io.IOStyle
import net.jackadull.dirish.op.settings.DirishSettingStyle

import scala.language.higherKinds

package object locking {
  type LockingStyle[V[+_,+_]] = CombinatorStyle[V] with DirishSettingStyle[V] with IOStyle[V]
}
