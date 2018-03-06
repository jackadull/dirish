package net.jackadull.dirish.op.signals

import java.lang.System.currentTimeMillis

import net.jackadull.dirish.op.combinator.{BlockingEitherCombinatorStyle, CombinatorStyle, EitherV}
import net.jackadull.dirish.op.signals.SignalStyle.SignalGet
import net.jackadull.dirish.op.util.UsingCombinator

import scala.concurrent.{Await, Promise, TimeoutException}
import scala.language.{higherKinds, postfixOps}
import scala.util.Success

class BlockingSignalStyle[V[+_,+_]](protected val combinatorStyle:CombinatorStyle[V]) extends SignalStyle[V] with UsingCombinator[V] {
  private var cache:Map[Any,Promise[(Long,Any)]] = Map()
  private val mutex = new Object

  def signal[R,E](get:SignalGet[R,E,V], caching:SignalCacheConfig):V[R,E] = newOrExistingPromise(get) match {
    case Right(existingPromise) ⇒
      try {
        val (resultCreation, resultV) = Await.result(existingPromise future, caching cacheDuration)
        val resultAge = currentTimeMillis() - resultCreation
        val maxAge =
          (if(combinatorStyle isDeterminedAsFailed resultV.asInstanceOf[V[_,_]]) caching.errorCacheDuration else caching.cacheDuration).toMillis
        if(resultAge > maxAge) {deleteFromCache(get, existingPromise); signal(get, caching)}
        else resultV.asInstanceOf[V[R,E]]
      } catch {
        case _:TimeoutException ⇒ deleteFromCache(get, existingPromise); signal(get, caching)
      }
    case Left(newPromise) ⇒
      try {val resultV = get(); newPromise complete Success((currentTimeMillis(), resultV)); resultV}
      catch {case throwable:Throwable ⇒ newPromise failure throwable; throw throwable}
  }

  private def newOrExistingPromise(key:Any):Either[Promise[(Long,Any)],Promise[(Long,Any)]] = mutex synchronized {cache get key match {
    case Some(promise) ⇒ Right(promise)
    case None ⇒
      val newPromise = Promise[(Long,Any)]()
      cache += key → newPromise
      Left(newPromise)
  }}

  private def deleteFromCache(key:Any, value:Promise[(Long,Any)]) {mutex synchronized {cache get key match {
    case Some(`value`) ⇒ cache -= key
    case _ ⇒ ()
  }}}
}
object BlockingSignalStyle extends BlockingSignalStyle[EitherV](BlockingEitherCombinatorStyle)
