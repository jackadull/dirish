package net.jackadull.dirish.op.network

import net.jackadull.dirish.op.OpError

sealed trait NetworkError extends OpError

sealed trait IsHostReachableError extends NetworkError

trait GenericNetworkError extends IsHostReachableError with NetworkError

final case class UnknownHost(msg:String, host:String) extends IsHostReachableError