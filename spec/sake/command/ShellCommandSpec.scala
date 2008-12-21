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
    
    "Running a ShellCommand with a string" should {
        "convert the string to 'command -> first_word and 'opts -> rest_of_string" in {
            val c = new ShellCommand("withString", Map('opts -> "boo", 'cp -> "a:b")) {
                override def action(opts: Map[Symbol,Any]) = {
                    opts.keys.foreach { key => 
                        key match {
                            case 'command => opts(key) must be_==("echo")
                            case 'opts => opts(key) must be_==(List("hello", "world"))
                            case 'cp => opts(key) must be_==("a:b")
                            case _ => fail(key.toString())
                        }
                    }
                    new Passed()
                }
            }
            c("echo hello world")            
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
             checkString("""shcmd\s+-foo foo -bar bar1[:;]bar2""".r, byteStream.toString())
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
             checkString("""shcmd\s+-foo foobar -bar bar1[:;]bar2 -baz \(a,b\)""".r, byteStream.toString())
        }        
    }
    
    "Running a ShellCommand with standard options" should {
        doBefore {
            byteStream  = new ByteArrayOutputStream()
            newStream   = new PrintStream(byteStream)
            Log.log.out = newStream
        }

        "map 'files -> file_spec to the list of files matching the spec." in {
            val cmd = new ShellCommand("shcmd") {
                override def makeFilesLister = FakeFileForSpecs.fakeFilesFinder
            }
            cmd('files -> "foo/**/*Spec.class")
            val actual = byteStream.toString()
            val expected = FakeFileForSpecs.fakeFilesExpected.reduceLeft(_+" "+_)
            checkString(("""shcmd\s+(?!-files)\s*"""+expected).r, byteStream.toString())
        }

        "map 'command -> string to use 'string' as the shell command name" in {
             val cmd = new ShellCommand("shcmd")
             cmd('command -> "java", 'opts -> "-cp foo:bar -Dx=y -d -g:1")
             checkString("""java\s+-cp foo[:;]bar -Dx=y -d -g:1""".r, byteStream.toString())
        }

        "map 'opts -> string to a string with each word (split on whitespace)" in {
             val cmd = new ShellCommand("shcmd")
             cmd('command -> "shcmd", 'opts -> "-x foo:bar -Dx=y -d -g:1 -x'y z'")
             checkString("""shcmd\s+-x foo:bar -Dx=y -d -g:1 -x'y z'\s*""".r, byteStream.toString())
        }

        "map 'opts -> List[String] to a string with each element of the list" in {
             val cmd = new ShellCommand("shcmd")
             cmd('command -> "shcmd", 'opts -> List("-x", "foo:bar", "-Dx=y", "-d", "-g:1", "-x'y z'"))
             checkString("""shcmd\s+-x foo:bar -Dx=y -d -g:1 -x'y z'\s*""".r, byteStream.toString())
        }

        "map any other unknown 'opt -> List(a,b,c) to a path-like '-opt -> a:b:c'" in {
             val cmd = new ShellCommand("shcmd")
             cmd('command -> "shcmd", 'foo -> List("a", "b", "c"))
             checkString("""shcmd\s+-foo a[:;]b[:;]c""".r, byteStream.toString())
        }

        "map any other unknown 'opt -> Any to '-opt -> Any.toString()' (without quotes)" in {
             val cmd = new ShellCommand("shcmd")
             cmd('command -> "shcmd", 'foo -> ("a", "b", "c"))
             checkString("""shcmd\s+-foo \(a,b,c\)""".r, byteStream.toString())
        }
    }
    
}