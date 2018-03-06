package net.jackadull.dirish.op.settings

import net.jackadull.dirish.op.OpError

import scala.language.higherKinds

trait DirishSettingStyleProxy[V[+_,+_]] extends DirishSettingStyle[V] {
  protected def settingsStyle:DirishSettingStyle[V]

  def getDirishSetting[A](setting:DirishSetting[A]):V[A,OpError] = settingsStyle getDirishSetting setting
}
