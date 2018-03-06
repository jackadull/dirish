package net.jackadull.dirish.op.network

import java.net._

import net.jackadull.dirish.op.GenericThrowableError
import net.jackadull.dirish.op.combinator.{BlockingEitherCombinatorStyle, CombinatorStyle, EitherV}
import net.jackadull.dirish.op.util.UsingCombinator

import scala.language.{higherKinds, reflectiveCalls}

class BlockingNetworkStyle[V[+_,+_]](protected val combinatorStyle:CombinatorStyle[V])
extends NetworkStyle[V] with UsingCombinator[V] {
  def canConnectToHost(host:String, port:Int, timeoutMillis:Int):V[Boolean,CanConnectToHostError] =
    vtry({
      val socket = new Socket()
      try {
        try {socket connect (new InetSocketAddress(host, port), timeoutMillis); true}
        catch {case _:SocketTimeoutException | _:UnknownHostException ⇒ false}
      } finally {socket close()}
    }, {case t ⇒ GenericThrowableError(s"Cannot check if connection is possible to $host:$port", t)})

  def isHostReachable(host:String, timeoutMillis:Int):V[Boolean,IsHostReachableError] =
    vtry(
      {InetAddress.getByName(host).isReachable(timeoutMillis)},
      {
        case _:UnknownHostException ⇒ UnknownHost(s"Host $host not reachable", host)
        case t ⇒ GenericThrowableError(s"Cannot check if host $host is reachable", t)
      })
}
object BlockingNetworkStyle extends BlockingNetworkStyle[EitherV](BlockingEitherCombinatorStyle)
