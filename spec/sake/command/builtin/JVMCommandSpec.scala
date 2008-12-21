package sake.command.builtin

import org.specs._
import scala.util.matching.Regex
import sake.util._
import sake.environment._
import java.io.{PrintStream, ByteArrayOutputStream}

object JVMCommandSpec extends Specification { 
    val savedDryRun = Environment.environment.dryRun
    val savedLog    = Log.log
    var byteStream  = new ByteArrayOutputStream()
    var newStream   = new PrintStream(byteStream)
    val delim = Environment.environment.pathSeparator
    
    def checkString(regex: Regex, actual: String) = {
        (regex findFirstIn actual) match {
            case Some(s) =>
            case None => fail("expected (regex): "+regex+", actual: "+actual)
        }
    }
    
    doBeforeSpec {
        Environment.environment.dryRun = true
        Log.log = new Log(Level.Info, newStream)
    }
    
    doAfterSpec {
        Environment.environment.dryRun = savedDryRun
        Log.log = savedLog
    }

    "Running a JVMCommand with standard options" should {
        doBefore {
            byteStream  = new ByteArrayOutputStream()
            newStream   = new PrintStream(byteStream)
            Log.log.out = newStream
        }

        "maps 'classpath -> List(a,b,c) to '-cp a:b:c'" in {
             val cmd = new JVMCommand("java", Map('classpath -> List("bar1", "bar2", "bar3")))
             cmd()
             checkString("""java\s+-cp bar1[:;]bar2[:;]bar3""".r, byteStream.toString())
        }        

        "maps 'cp -> List(a,b,c) to '-cp a:b:c'" in {
             val cmd = new JVMCommand("java", Map('classpath -> List("bar1", "bar2", "bar3")))
             cmd()
             checkString("""java\s+-cp bar1[:;]bar2[:;]bar3""".r, byteStream.toString())
        }        

        "maps 'class -> name to 'name'" in {
             val cmd = new JVMCommand("java", Map('class -> "foo.bar.Bar"))
             cmd()
             checkString("""java\s+foo.bar.Bar""".r, byteStream.toString())
        }        
    }
}