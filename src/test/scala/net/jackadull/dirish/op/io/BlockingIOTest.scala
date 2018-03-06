package net.jackadull.dirish.op.io

import java.nio.file.{Files, Paths}

import net.jackadull.dirish.op.combinator.{BlockingEitherCombinatorStyle, EitherV}
import org.scalatest.FreeSpec

import scala.language.{higherKinds, postfixOps, reflectiveCalls}

// TODO remove this as soon as style combinators are available
class TestBlockingIO(val userHome:String)
extends BlockingIOStyle[({type EitherInv[+A,+B] = Either[B,A]})#EitherInv](BlockingEitherCombinatorStyle, userHome) with BlockingEitherCombinatorStyle

class BlockingIOTest extends FreeSpec with GenericIOTest {
  type TestIOV[+R,+E] = EitherV[R,E]
  type TestIOStyle = TestBlockingIO
  type TestIOContext = TestIOStyle

  protected def createIO():TestBlockingIO = new TestBlockingIO(Files.createTempDirectory("blocking-io-test") toString)
  protected def getIOStyle(context:TestBlockingIO):TestBlockingIO = context
  protected def ioImplementationName:String = "BlockingIOStyle"
  protected def tearDownIO(context:TestBlockingIO) {Files.delete(Paths get(context userHome))} // TODO delete recursively
}
