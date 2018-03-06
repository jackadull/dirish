package net.jackadull.dirish.op.network

import java.net.{InetAddress, UnknownHostException}

import net.jackadull.dirish.op.GenericThrowableError
import net.jackadull.dirish.op.combinator.{BlockingEitherCombinatorStyle, CombinatorStyle, EitherV}
import net.jackadull.dirish.op.util.UsingCombinator

import scala.language.{higherKinds, reflectiveCalls}

class BlockingNetworkStyle[V[+_,+_]](protected val combinatorStyle:CombinatorStyle[V])
extends NetworkStyle[V] with UsingCombinator[V] {
  def isHostReachable(host:String, timeoutMillis:Int):V[Boolean,IsHostReachableError] =
    vtry(
      {InetAddress.getByName(host).isReachable(timeoutMillis)},
      {
        case _:UnknownHostException ⇒ UnknownHost(s"Host $host not reachable", host)
        case t ⇒ GenericThrowableError(s"Cannot check if host $host is reachable", t)
      })
}
object BlockingNetworkStyle extends BlockingNetworkStyle[EitherV](BlockingEitherCombinatorStyle)
