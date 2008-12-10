package sake.command

import org.specs._
import scala.util.matching.Regex
import sake.util._
import sake.environment._
import java.io.{PrintStream, ByteArrayOutputStream}

object ShellCommandSpec extends Specification { 
    val savedDryRun = Environment.environment.dryRun
    val savedLog    = Log.log
    var byteStream  = new ByteArrayOutputStream()
    var newStream   = new PrintStream(byteStream)
    val delim = Environment.environment.pathSeparator
    
    def checkString(regex: Regex, actual: String) = {
        (regex findFirstIn actual) match {
            case Some(s) =>
            case None => fail(actual)
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
             checkString("""shcmd\s+-foo foo -bar "bar1[:;]bar2"\s*""".r, byteStream.toString())
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
             checkString("""shcmd\s+-foo foobar -bar "bar1[:;]bar2" -baz \(a,b\)""".r, byteStream.toString())
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
             checkString("""shcmd\s+-cp "bar1[:;]bar2[:;]bar3"\s*""".r, byteStream.toString())
        }        

        // TODO - currently just prints the specification string verbatim!
        "maps 'files -> file_spec to the list of files matching the spec. (ie., verbatim)" in {
             val cmd = new ShellCommand("shcmd")
             cmd('files -> "**/*.scala")
             val actual = byteStream.toString()
             checkString("""shcmd\s+(?!-files)\s*\*\*/\*.scala""".r, byteStream.toString())
        }        

        "maps 'command -> string to -c 'string' (i.e., for passing commands to a shell)" in {
             val cmd = new ShellCommand("shcmd")
             cmd('command -> "-cp foo:bar -Dx=y -d -g:1")
             checkString("""shcmd\s+-c '-cp foo[:;]bar -Dx=y -d -g:1'""".r, byteStream.toString())
        }        

        "maps 'opts -> string to 'string' (i.e., verbatim, with no added argument quoting)" in {
             val cmd = new ShellCommand("shcmd")
             cmd('opts -> "-cp foo:bar -Dx=y -d -g:1")
             checkString("""shcmd\s+-cp foo[:;]bar -Dx=y -d -g:1""".r, byteStream.toString())
        }        

        "maps any other unknown 'opt -> List(a,b,c) to a path-like '-opt -> \"a:b:c\"' (with quotes)" in {
             val cmd = new ShellCommand("shcmd")
             cmd('foo -> List("a", "b", "c"))
             checkString("""shcmd\s+-foo "a[:;]b[:;]c"\s*""".r, byteStream.toString())
        }        

        "maps any other unknown 'opt -> Any to '-opt -> Any.toString()' (without quotes)" in {
             val cmd = new ShellCommand("shcmd")
             cmd('foo -> ("a", "b", "c"))
             checkString("""shcmd\s+-foo \(a,b,c\)""".r, byteStream.toString())
        }        

        "maps any one of 'f, 'r, 'rf to itself (i.e., as in \"rm -rf\" options)" in {
             val cmd = new ShellCommand("shcmd")
             List('f, 'r, 'rf)foreach { opt =>
                 cmd(opt -> "hello world!")
                 checkString(("shcmd\\s+-"+opt.name).r, byteStream.toString())
             }
        }        

        "maps 'force to -f (i.e., as in \"rm -f\")" in {
             val cmd = new ShellCommand("shcmd")
             cmd('force -> "hello world!")
             checkString("""shcmd\s+-f""".r, byteStream.toString())
        }

        "maps 'recursive to -r (i.e., as in \"rm -r\")" in {
             val cmd = new ShellCommand("shcmd")
             cmd('recursive -> "hello world!")
             checkString("""shcmd\s+-r""".r, byteStream.toString())
        }
    }
    
}