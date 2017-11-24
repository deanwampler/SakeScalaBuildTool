package sake.command.builtin

import org.scalatest._
import org.scalatest.Matchers._
import scala.util.matching.Regex
import sake.util._
import sake.context._
import java.io.{PrintStream, ByteArrayOutputStream}

object JVMCommandSpec extends FreeSpec {
    val savedDryRun = Environment.dryRun
    val savedLog    = Log.log
    var byteStream  = new ByteArrayOutputStream()
    var newStream   = new PrintStream(byteStream)
    val delim = Environment.pathSeparator

    def checkString(regex: Regex, actual: String) = {
        (regex findFirstIn actual) match {
            case Some(s) =>
            case None => fail("expected (regex): "+regex+", actual: "+actual)
        }
    }

    doBeforeSpec {
        Environment.dryRun = true
        Log.log = new Log(Level.Info, newStream)
    }

    doAfterSpec {
        Environment.dryRun = savedDryRun
        Log.log = savedLog
    }

    "Running a JVMCommand with standard options" should {
        doBefore {
            byteStream  = new ByteArrayOutputStream()
            newStream   = new PrintStream(byteStream)
            Log.log.out = newStream
        }

        "maps 'classpath -> List(a,b,c) to -classpath a:b:c'" in {
             val cmd = new JVMCommand("java", Map('classpath -> List("bar1", "bar2", "bar3")))
             cmd()
             byteStream.toString() must be matching("""java\s+-classpath bar1[:;]bar2[:;]bar3[:;].*""")
        }

        "maps 'cp -> List(a,b,c) to -classpath a:b:c'" in {
             val cmd = new JVMCommand("java", Map('classpath -> List("bar1", "bar2", "bar3")))
             cmd()
             byteStream.toString() must be matching("""java\s+-classpath bar1[:;]bar2[:;]bar3[:;].*""")
        }

        "maps 'class -> name to 'name'" in {
             val cmd = new JVMCommand("java", Map('class -> "foo.bar.Bar"))
             cmd()
             byteStream.toString() must be matching("""java\s+foo.bar.Bar""")
        }
    }
}
