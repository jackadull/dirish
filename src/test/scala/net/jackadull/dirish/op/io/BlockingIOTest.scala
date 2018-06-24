package net.jackadull.dirish.op.io

import java.nio.file.{Files, Path, Paths}

import net.jackadull.dirish.op.combinator.{BlockingEitherCombinatorStyle, EitherV}
import org.scalatest.FreeSpec

import scala.language.{higherKinds, postfixOps, reflectiveCalls}

class TestBlockingIO(val userHome:String)
extends BlockingIOStyle[({type EitherInv[+A,+B] = Either[B,A]})#EitherInv](BlockingEitherCombinatorStyle, userHome) with BlockingEitherCombinatorStyle

class BlockingIOTest extends FreeSpec with GenericIOTest {
  type TestIOV[+R,+E] = EitherV[R,E]
  type TestIOStyle = TestBlockingIO
  type TestIOContext = TestIOStyle

  protected def createIO():TestBlockingIO = new TestBlockingIO(Files.createTempDirectory("blocking-io-test") toString)
  protected def getIOStyle(context:TestBlockingIO):TestBlockingIO = context
  protected def ioImplementationName:String = "BlockingIOStyle"
  protected def tearDownIO(context:TestBlockingIO) {
    def deleteRecursively(p:Path) {
      Files.list(p).forEach(deleteRecursively)
      Files.delete(p)
    }
    deleteRecursively(Paths get(context userHome))
  }
}
