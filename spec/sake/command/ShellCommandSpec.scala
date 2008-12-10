package sake.command

import org.specs._
import sake.util._
import sake.environment._
import java.io.{PrintStream, ByteArrayOutputStream}

object ShellCommandSpec extends Specification { 
    val savedDryRun = Environment.environment.dryRun
    val savedLog    = Log.log
    var byteStream  = new ByteArrayOutputStream()
    var newStream   = new PrintStream(byteStream)
    val delim = Environment.environment.pathSeparator
    
    doBeforeSpec {
        Environment.environment.dryRun = true
        Log.log = new Log(Level.Info, newStream)
    }
    
    doAfterSpec {
        Environment.environment.dryRun = savedDryRun
        Log.log = savedLog
    }

    "A new ShellCommand created with just a name" should {
        "have the same name as the shell command invoked." in {
             val cmd = new ShellCommand("shcmd")
             cmd.name must be_==("shcmd")
             cmd.defaultOptions must be_==(None)
        }        
    }
    
    "A new ShellCommand created with a name and one (A,B) option" should {
        "have the same name as the shell command invoked." in {
             val cmd = new ShellCommand("shcmd", 'foo -> "foo")
             cmd.name must be_==("shcmd")
        }        

        "have the specified options in the default options map." in {
             val cmd = new ShellCommand("shcmd", 'foo -> "foo")
             cmd.defaultOptions match {
                 case None => fail()
                 case Some(map) => map must be_==(Map('foo -> "foo"))
             } 
        }        
    }
    
    "A new ShellCommand created with a name and multiple (A,B) options" should {
        "have the same name as the shell command invoked." in {
             val cmd = new ShellCommand("shcmd", 'foo -> "foo", 'bar -> "bar")
             cmd.name must be_==("shcmd")
        }        

        "have the specified options in the default options map." in {
             val cmd = new ShellCommand("shcmd", 'foo -> "foo", 'bar -> "bar")
             cmd.defaultOptions match {
                 case None => fail()
                 case Some(map) => map must be_==(Map('foo -> "foo", 'bar -> "bar"))
             }
        }
    }
    
    "A new ShellCommand created with a name and a Map of (A,B) options" should {
        "have the same name as the shell command invoked." in {
             val cmd = new ShellCommand("shcmd", Map('foo -> "foo", 'bar -> "bar"))
             cmd.name must be_==("shcmd")
             cmd.defaultOptions match {
                 case None => fail()
                 case Some(map) => map must be_==(Map('foo -> "foo", 'bar -> "bar"))
             } 
        }        

        "have the specified options in the default options map." in {
             val cmd = new ShellCommand("shcmd", Map('foo -> "foo", 'bar -> "bar"))
             cmd.defaultOptions match {
                 case None => fail()
                 case Some(map) => map must be_==(Map('foo -> "foo", 'bar -> "bar"))
             } 
        }        
    }
    
    "Running a ShellCommand without additional options" should {
        doBefore {
            byteStream  = new ByteArrayOutputStream()
            newStream   = new PrintStream(byteStream)
            Log.log.out = newStream
        }
        
        "compose the default options into a command string, starting with the command name." in {
             val cmd = new ShellCommand("shcmd", Map('foo -> "foo", 'bar -> List("bar1", "bar2")))
             cmd()
             val actual = byteStream.toString()
             // check command and arguments separately to ignore space...
             actual.contains("shcmd") must be_==(true)
             actual.contains("-foo \"foo\" -bar \"bar1"+delim+"bar2\"") must be_==(true)
        }        
    }        
    
    "Running a ShellCommand with additional options" should {
        doBefore {
            byteStream  = new ByteArrayOutputStream()
            newStream   = new PrintStream(byteStream)
            Log.log.out = newStream
        }

        "compose the additional and default options, overwriting the later into a command string, starting with the command name." in {
             val cmd = new ShellCommand("shcmd", Map('foo -> "foo", 'bar -> List("bar1", "bar2")))
             cmd('foo -> "foobar", 'baz -> ("a", "b"))
             val actual = byteStream.toString()
             actual.contains("shcmd") must be_==(true)
             actual.contains("-foo \"foobar\" -bar \"bar1"+delim+"bar2\" -baz \"(a,b)\"") must be_==(true)
        }        
    }
    
    "Running a ShellCommand with standard options" should {
        doBefore {
            byteStream  = new ByteArrayOutputStream()
            newStream   = new PrintStream(byteStream)
            Log.log.out = newStream
        }

        "maps 'classpath -> List(a,b,c) to '-cp \"a:b:c\"' (with quotes)" in {
             val cmd = new ShellCommand("shcmd", Map('classpath -> List("bar1", "bar2", "bar3")))
             cmd()
             val actual = byteStream.toString()
             actual.contains("shcmd") must be_==(true)
             actual.contains("-cp \"bar1:bar2:bar3\"") must be_==(true)
        }        

        // TODO - currently just prints the specification string verbatim!
        "maps 'files -> file_spec to the list of files matching the spec. (ie., verbatim)" in {
             val cmd = new ShellCommand("shcmd")
             cmd('files -> "**/*.scala")
             val actual = byteStream.toString()
             actual.contains("shcmd") must be_==(true)
             actual.contains("-files") must be_==(false)
             actual.contains("**/*.scala") must be_==(true)
        }        

        "maps any one of 'force, 'f, 'recursive, 'r, rf, 'opts -> string to 'string' (ie., verbatim, with no added argument quoting)" in {
             val cmd = new ShellCommand("shcmd")
             cmd('opts -> "-cp foo:bar -Dx=y -d -g:1")
             val actual = byteStream.toString()
             actual.contains("shcmd") must be_==(true)
             actual.contains("-cp foo:bar -Dx=y -d -g:1") must be_==(true)
        }        

        "maps another other 'opt -> List(a,b,c) to '-opt -> \"a:b:c\"' (with quotes)" in {
             val cmd = new ShellCommand("shcmd")
             cmd('foo -> List("a", "b", "c"))
             val actual = byteStream.toString()
             actual.contains("shcmd") must be_==(true)
             actual.contains("-foo \"a:b:c\"") must be_==(true)
        }        

        "maps another other 'opt -> obj to '-opt -> +\"obj.toString()\"  (with quotes)" in {
             val cmd = new ShellCommand("shcmd")
             cmd('foo -> ("a", "b", "c"))
             val actual = byteStream.toString()
             actual.contains("shcmd") must be_==(true)
             actual.contains("-foo \"(a,b,c)\"") must be_==(true)
        }        

    }
    
}