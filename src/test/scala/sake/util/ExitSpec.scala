package sake.util

import org.scalatest._
import org.scalatest.Matchers._

object ExitSpec extends FreeSpec {
    val oldLog = Log.default

    doBeforeSpec {
        // Suppress stdout/stderr output...
        import java.io.{PrintStream, ByteArrayOutputStream}
        val newStream = new PrintStream(new ByteArrayOutputStream())
        Log.default = new Log(Level.Warn, newStream)
    }

    doAfterSpec {
        Log.default = oldLog
    }

    "The error method with just a message" should {
        "throw a BuildError with the message" in {
            try {
                Exit.error("message")
                fail()
            } catch {
                case BuildError(msg, th) =>  msg shouldEqual "message"
                case _ => fail()
            }
        }
    }

    "The error method with a message and a Throwable" should {
        "throw a BuildError with the message and the throwable" in {
            val ex = new Exception()
            try {
                Exit.error("message", ex)
                fail()
            } catch {
                case BuildError(msg, th) =>  {
                    msg shouldEqual "message"
                    (th eq ex) shouldEqual true
                }
                case _ => fail()
            }
        }
    }
}
