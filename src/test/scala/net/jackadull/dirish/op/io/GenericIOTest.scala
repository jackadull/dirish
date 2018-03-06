package net.jackadull.dirish.op.io

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference

import net.jackadull.dirish.op.combinator.{CombinatorStyle, FailWith, ResultIn}
import net.jackadull.dirish.op.{GenericMessageError, Op}
import net.jackadull.dirish.path.UserHomePathSpec
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers}

import scala.language.{higherKinds, postfixOps, reflectiveCalls}

trait GenericIOTest extends Matchers with BeforeAndAfterAll {
  this:FreeSpec ⇒

  type TestIOV[+_,+_]
  type TestIOStyle<:(CombinatorStyle[TestIOV] with IOStyle[TestIOV])
  type TestIOContext<:AnyRef

  protected def createIO():TestIOContext
  protected def getIOStyle(context:TestIOContext):TestIOStyle
  protected def ioImplementationName:String
  protected def tearDownIO(context:TestIOContext)

  private val home = UserHomePathSpec

  ioImplementationName - {
    "creating and then moving a simple directory to trash works" in {
      val testDirectory = home/"simple-test-directory"

      val op = (CreateDirectory(testDirectory) ~> FileKindInfo(testDirectory) >> {
        case IsDirectory ⇒ ResultIn.success
        case unexpected ⇒ FailWith(GenericMessageError(s"Expected IsDirectory instead of: $unexpected"))
      }) ~> MoveFileToTrash(testDirectory) ~> FileKindInfo(testDirectory) >> {
        case IsNonExistent ⇒ ResultIn.success
        case unexpected ⇒ FailWith(GenericMessageError(s"Expected IsNonExistent instead of: $unexpected"))
      }

      executionResult(op) match {
        case Left(error) ⇒ fail(error toString)
        case Right(_) ⇒ succeed
      }
    }
  }

  def io:TestIOStyle = ioContainer get() getOrElse(sys error "Called outside of a test (no IO instance)")
  private val ioContainer:AtomicReference[Option[TestIOStyle]] = new AtomicReference(None)
  private val ioContextContainer:AtomicReference[Option[TestIOContext]] = new AtomicReference(None)

  override protected def afterAll() = {
    ioContextContainer get() match {
      case Some(ctx) ⇒ tearDownIO(ctx); ioContextContainer set None; ioContainer set None
      case None ⇒ ioContextContainer set None; ioContainer set None
    }
    super.afterAll()
  }

  override protected def beforeAll() = {
    super.beforeAll()
    ioContextContainer set None; ioContainer set None
    val ctx = createIO()
    ioContextContainer set Some(ctx); ioContainer set Some(getIOStyle(ctx))
  }

  private def executionResult[R,E](op:Op[R,E,({type S2[V2[+_,+_]] = CombinatorStyle[V2] with IOStyle[V2]})#S2]):Either[E,R] = {
    val fut = new java.util.concurrent.CompletableFuture[Either[E,R]]()
    val op2 = op #>> {error ⇒
      fut complete Left(error); FailWith(error)
    } >> {result ⇒
      fut complete Right(result); ResultIn(result)
    }
    op2 instantiateIn io
    fut.get(5, TimeUnit.SECONDS)
  }
}
