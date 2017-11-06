package sake.command.builtin

import org.specs._
import scala.util.matching.Regex
import sake.util._
import sake.environment._
import java.io.{PrintStream, ByteArrayOutputStream}

object JVMCommandSpec extends Specification {
    val savedDryRun = Environment.default.dryRun
    val savedLog    = Log.log
    var byteStream  = new ByteArrayOutputStream()
    var newStream   = new PrintStream(byteStream)
    val delim = Environment.default.pathSeparator

    def checkString(regex: Regex, actual: String) = {
        (regex findFirstIn actual) match {
            case Some(s) =>
            case None => fail("expected (regex): "+regex+", actual: "+actual)
        }
    }

    doBeforeSpec {
        Environment.default.dryRun = true
        Log.log = new Log(Level.Info, newStream)
    }

    doAfterSpec {
        Environment.default.dryRun = savedDryRun
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
