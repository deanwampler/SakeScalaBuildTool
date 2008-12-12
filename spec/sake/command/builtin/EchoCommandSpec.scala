package sake.command.builtin

import org.specs._
import scala.util.matching.Regex
import sake.util._
import sake.environment._
import java.io.{PrintStream, ByteArrayOutputStream}

object EchoCommandSpec extends Specification { 
    val savedLog    = Log.log
    var byteStream  = new ByteArrayOutputStream()
    var newStream   = new PrintStream(byteStream)
    
    doBeforeSpec {
        Log.log = new Log(Level.Notice, newStream)
    }
    
    doAfterSpec {
        Log.log = savedLog
    }
    
    def checkString(expectedSubStr:String, actual:String) = actual.contains(expectedSubStr) match {
        case false => fail("actual ("+actual+") doesn't contain expected ("+expectedSubStr+")")
        case true =>
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
             checkString("Notice: now is the time", byteStream.toString())
        }        

        "accepts a string argument to print" in {
             val cmd = new EchoCommand()
             cmd("now is the time")
             checkString("Notice: now is the time", byteStream.toString())
        }        

        "outputs the text to the log" in {
             val cmd = new EchoCommand()
             cmd("now is the time")
             checkString("Notice: now is the time", byteStream.toString())
        }        

        "outputs nothing if the default log level is below the threshold" in {
             val cmd = new EchoCommand(Level.Info)
             cmd("now is the time")
             checkString("", byteStream.toString())
             byteStream.toString() must be_==("")
        }        

        "overrides the default log level if 'level => level is specified" in {
             val cmd = new EchoCommand(Level.Info)
             cmd('level -> Level.Warn, 'words -> "now is the time")
             checkString("Warn: now is the time", byteStream.toString())
        }        
    }
}