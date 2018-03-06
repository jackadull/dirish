package net.jackadull.dirish.op.settings

import net.jackadull.dirish.op.OpError

import scala.language.higherKinds

trait DirishSettingStyle[V[+_,+_]] {
  def getDirishSetting[A](setting:DirishSetting[A]):V[A,OpError]
}
