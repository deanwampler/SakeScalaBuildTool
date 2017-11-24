package sake.command.builtin

import org.scalatest._
import org.scalatest.Matchers._
import scala.util.matching.Regex
import sake.util._
import sake.context._
import java.io.{PrintStream, ByteArrayOutputStream}

object EchoCommandSpec extends FreeSpec {
    val savedLog    = Log.log
    var byteStream  = new ByteArrayOutputStream()
    var newStream   = new PrintStream(byteStream)

    doBeforeSpec {
        Log.log = new Log(Level.Notice, newStream)
    }

    doAfterSpec {
        Log.log = savedLog
    }

    "Running an EchoCommand" should {
        doBefore {
            byteStream  = new ByteArrayOutputStream()
            newStream   = new PrintStream(byteStream)
            Log.log.out = newStream
        }

        "maps 'words -> any to 'any.toString()'" in {
             val cmd = new EchoCommand()
             cmd('words -> "now is the time")
             byteStream.toString must be matching ("Notice: now is the time")
        }

        "accepts a string argument to print" in {
             val cmd = new EchoCommand()
             cmd("now is the time")
             byteStream.toString must be matching ("Notice: now is the time")
        }

        "outputs the text to the log" in {
             val cmd = new EchoCommand()
             cmd("now is the time")
             byteStream.toString must be matching ("Notice: now is the time")
        }

        "outputs nothing if the default log level is below the threshold" in {
             val cmd = new EchoCommand(Level.Info)
             cmd("now is the time")
             byteStream.toString() shouldEqual ""
        }

        "overrides the default log level if 'level => level is specified" in {
             val cmd = new EchoCommand(Level.Info)
             cmd('level -> Level.Warn, 'words -> "now is the time")
             byteStream.toString must be matching ("Warn: now is the time")
        }
    }
}
