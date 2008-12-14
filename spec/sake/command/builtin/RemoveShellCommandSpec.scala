package sake.command.builtin

import org.specs._
import scala.util.matching.Regex
import sake.util._
import sake.environment._
import java.io.{PrintStream, ByteArrayOutputStream}

object RemoveShellCommandSpec extends Specification { 
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

    "Running a RemoveShellCommand with standard options" should {
        doBefore {
            byteStream  = new ByteArrayOutputStream()
            newStream   = new PrintStream(byteStream)
            Log.log.out = newStream
        }

        "maps 'recursive -> true to '-rf'" in {
            val cmd = new RemoveShellCommand("remove", 
                Map('recursive -> true, 'files -> "foo/bar1/**/*Spec.class")) {
                    override def makeFilesLister = FakeFileForSpecs.oneFakeFile
                }
            cmd()
            val expected = FakeFileForSpecs.oneFakeFileExpected.reduceLeft(_+" "+_)
            checkString(("""remove\s+-rf """+expected).r, byteStream.toString())
        }        

        "maps 'recursive -> false to ''" in {
            val cmd = new RemoveShellCommand("remove", 
                Map('recursive -> false, 'files -> "foo/bar1/**/*Spec.class")) {
                    override def makeFilesLister = FakeFileForSpecs.oneFakeFile
                }
            cmd()
            val expected = FakeFileForSpecs.oneFakeFileExpected.reduceLeft(_+" "+_)
            checkString(("""remove\s+"""+expected).r, byteStream.toString())
        }        

        "maps 'force -> true to '-f'" in {
            val cmd = new RemoveShellCommand("remove", 
                Map('force -> true, 'files -> "foo/bar1/**/*Spec.class")) {
                    override def makeFilesLister = FakeFileForSpecs.oneFakeFile
                }
            cmd()
            val expected = FakeFileForSpecs.oneFakeFileExpected.reduceLeft(_+" "+_)
            checkString(("""remove\s+-f """+expected).r, byteStream.toString())
        }        

        "maps 'force -> false to ''" in {
            val cmd = new RemoveShellCommand("remove", 
                Map('force -> false, 'files -> "foo/bar1/**/*Spec.class")) {
                    override def makeFilesLister = FakeFileForSpecs.oneFakeFile
                }
            cmd()
            val expected = FakeFileForSpecs.oneFakeFileExpected.reduceLeft(_+" "+_)
            checkString(("""remove\s+"""+expected).r, byteStream.toString())
        }        
    }
}