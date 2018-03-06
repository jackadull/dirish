package net.jackadull.dirish.op.settings

import net.jackadull.dirish.op.{Op, OpError}

import scala.language.higherKinds

final case class GetSetting[A](setting:DirishSetting[A]) extends Op[A,OpError,DirishSettingStyle] {
  def instantiateIn[V[+_,+_]](style:DirishSettingStyle[V]):V[A,OpError] = style getDirishSetting setting
}
