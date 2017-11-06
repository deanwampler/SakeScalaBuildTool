package sake.command

import org.specs._
import scala.util.matching.Regex
import sake.util._
import sake.environment._
import java.io.{PrintStream, ByteArrayOutputStream}

object ShellCommandSpec extends Specification {
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
        Log.log = new Log(Level.Info, newStream)
    }

    doAfterSpec {
        Log.log = savedLog
    }

    "A new ShellCommand created with just a name" should {
        "have the same name as the shell command invoked." in {
             val cmd = new ShellCommand("shcmd")
             cmd.name mustEqual "shcmd"
             cmd.defaultOptions mustEqual None
        }
    }

    "A new ShellCommand created with a name and one (A,B) option" should {
        "have the same name as the shell command invoked." in {
             val cmd = new ShellCommand("shcmd", 'foo -> "foo")
             cmd.name mustEqual "shcmd"
        }

        "have the specified options in the default options map." in {
             val cmd = new ShellCommand("shcmd", 'foo -> "foo")
             cmd.defaultOptions match {
                 case None => fail()
                 case Some(map) => map mustEqual Map('foo -> "foo")
             }
        }
    }

    "A new ShellCommand created with a name and multiple (A,B) options" should {
        "have the same name as the shell command invoked." in {
             val cmd = new ShellCommand("shcmd", 'foo -> "foo", 'bar -> "bar")
             cmd.name mustEqual "shcmd"
        }

        "have the specified options in the default options map." in {
             val cmd = new ShellCommand("shcmd", 'foo -> "foo", 'bar -> "bar")
             cmd.defaultOptions match {
                 case None => fail()
                 case Some(map) => map mustEqual Map('foo -> "foo", 'bar -> "bar")
             }
        }
    }

    "A new ShellCommand created with a name and a Map of (A,B) options" should {
        "have the same name as the shell command invoked." in {
             val cmd = new ShellCommand("shcmd", Map('foo -> "foo", 'bar -> "bar"))
             cmd.name mustEqual "shcmd"
             cmd.defaultOptions match {
                 case None => fail()
                 case Some(map) => map mustEqual Map('foo -> "foo", 'bar -> "bar")
             }
        }

        "have the specified options in the default options map." in {
             val cmd = new ShellCommand("shcmd", Map('foo -> "foo", 'bar -> "bar"))
             cmd.defaultOptions match {
                 case None => fail()
                 case Some(map) => map mustEqual Map('foo -> "foo", 'bar -> "bar")
             }
        }
    }
    "Running a ShellCommand with a string" should {
        "convert the string to 'command -> first_word and 'opts -> rest_of_string" in {
            ShellCommand.tokenizeCommandString("echo hello world") must containAll ("echo" :: List("hello", "world"))
        }

        """remove embedded \n and/or \r and other white space when tokenizing the command string""" in {
            ShellCommand.tokenizeCommandString("""echo
                    hello world""") must containAll ("echo" :: List("hello", "world"))

        }
    }

    "Running a ShellCommand without additional options" should {
        doBefore {
            byteStream  = new ByteArrayOutputStream()
            newStream   = new PrintStream(byteStream)
            Log.log.out = newStream
            Environment.default.dryRun = true
        }
        doAfter {
            Environment.default.dryRun = savedDryRun
        }

        "compose the default options into a command string, starting with the command name." in {
             val cmd = new ShellCommand("shcmd", Map('foo -> "foo", 'bar -> List("bar1", "bar2")))
             cmd()
             byteStream.toString() must be matching ("""shcmd\s+-foo foo -bar bar1[:;]bar2""")
        }
    }

    "Running a ShellCommand with additional options" should {
        doBefore {
            byteStream  = new ByteArrayOutputStream()
            newStream   = new PrintStream(byteStream)
            Log.log.out = newStream
            Environment.default.dryRun = true
        }
        doAfter {
            Environment.default.dryRun = savedDryRun
        }

        "compose the additional and default options, overwriting the later into a command string, starting with the command name." in {
             val cmd = new ShellCommand("ls", Map('foo -> "foo", 'bar -> List("bar1", "bar2")))
             cmd('foo -> "foobar", 'baz -> ("a", "b"))
             byteStream.toString() must be matching("""ls\s+-foo foobar -bar bar1[:;]bar2 -baz \(a,b\)""")
        }
    }

    "Running a ShellCommand with standard options" should {
        doBefore {
            byteStream  = new ByteArrayOutputStream()
            newStream   = new PrintStream(byteStream)
            Log.log.out = newStream
            Environment.default.dryRun = true
        }
        doAfter {
            Environment.default.dryRun = savedDryRun
        }

        "map 'files -> file_spec to the list of files matching the spec." in {
            val cmd = new ShellCommand("shcmd") {
                override def makeFilesLister = FakeFileForSpecs.fakeFilesFinder
            }
            cmd('files -> "foo/**/*Spec.class")
            val actual = byteStream.toString()
            val expected = FakeFileForSpecs.fakeFilesExpected.reduceLeft(_+" "+_)
            byteStream.toString() must be matching ("""shcmd\s+(?!-files)\s*""" + expected)
        }

        "map 'command -> string to use 'string' as the shell command name" in {
             val cmd = new ShellCommand("shcmd")
             cmd('command -> "java", 'opts -> "-classpath foo:bar -Dx=y -d -g:1")
             byteStream.toString() must be matching ("""java\s+-classpath foo[:;]bar -Dx=y -d -g:1""")
        }

        "map 'opts -> string to a string with each word (split on whitespace)" in {
             val cmd = new ShellCommand("shcmd")
             cmd('command -> "shcmd", 'opts -> "-x foo:bar -Dx=y -d -g:1 -x'y z'")
             byteStream.toString() must be matching ("""shcmd\s+-x foo:bar -Dx=y -d -g:1 -x'y z'\s*""")
        }

        "map 'opts -> List[String] to a string with each element of the list" in {
             val cmd = new ShellCommand("shcmd")
             cmd('command -> "shcmd", 'opts -> List("-x", "foo:bar", "-Dx=y", "-d", "-g:1", "-x'y z'"))
             byteStream.toString() must be matching ("""shcmd\s+-x foo:bar -Dx=y -d -g:1 -x'y z'\s*""")
        }

        "map 'directory -> String to the working directory the shell command should use" in {
             val cmd = new ShellCommand("shcmd") {
                 override def makeCommandRunner(command: String, args: List[String], options: Option[Map[Any,Any]]) = {
                     val runner = super.makeCommandRunner(command, args, options)
                     runner.processBuilder.directory().getName() mustEqual "lib"
                     runner
                 }
             }
             cmd('command -> "pwd", 'directory -> "lib")
        }

        "map 'D -> key=value to an environment variable named 'key' with value 'value'" in {
             val cmd = new ShellCommand("shcmd") {
                 override def makeCommandRunner(command: String, args: List[String], options: Option[Map[Any,Any]]) = {
                     val runner = super.makeCommandRunner(command, args, options)
                     runner.processBuilder.environment().get("key1") mustEqual "value1"
                     runner
                 }
             }
             cmd('command -> "pwd", 'D -> "key1=value1")
        }

        "map 'D -> key to an environment variable named 'key' and an empty 'value'" in {
             val cmd = new ShellCommand("shcmd") {
                 override def makeCommandRunner(command: String, args: List[String], options: Option[Map[Any,Any]]) = {
                     val runner = super.makeCommandRunner(command, args, options)
                     runner.processBuilder.environment().get("key1") mustEqual ""
                     runner
                 }
             }
             cmd('command -> "pwd", 'D -> "key1")
        }

        "throw a BuildError if 'D -> =value is given" in {
             val cmd = new ShellCommand("shcmd")
             cmd('command -> "pwd", 'D -> "=value1") must throwA[BuildError]
        }

        "map any other unknown 'opt -> List(a,b,c) to a path-like '-opt -> a:b:c'" in {
             val cmd = new ShellCommand("shcmd")
             cmd('command -> "shcmd", 'foo -> List("a", "b", "c"))
             byteStream.toString() must be matching ("""shcmd\s+-foo a[:;]b[:;]c""")
        }

        "map any other unknown 'opt -> Any to '-opt -> Any.toString()' (without quotes)" in {
             val cmd = new ShellCommand("shcmd")
             cmd('command -> "shcmd", 'foo -> ("a", "b", "c"))
             byteStream.toString() must be matching ("""shcmd\s+-foo \(a,b,c\)""")
        }
    }

    "Running a ShellCommand with standard file I/O options" should {
        doBefore {
            Environment.default.dryRun = false
        }
        doAfter {
            Environment.default.dryRun = savedDryRun
        }

        def doInputTextOutputFile {
            Environment.default.dryRun = false
            val outputFile = new FakeFile("toss.out")
            val cmd = new ShellCommand("shcmd")
            cmd('command -> "cat", 'inputText -> "hello world!", 'outputFile -> outputFile)
            outputFile.stringForReading mustEqual "hello world!\n"
        }

        "map 'inputText -> String to the input written to the subprocess" in {
             doInputTextOutputFile
        }

        def makeTempFileWithContent = {
            val tempFile = new FakeFile("tossInput.txt")
            tempFile.createNewFile
            val writer = tempFile.writer
            writer.write("hello world!")
            writer.flush()
            writer.close()
            tempFile
        }

        "map 'inputFile -> sake.util.File to the source of input written to the subprocess" in {
            Environment.default.dryRun = false
            val tempFile = makeTempFileWithContent
            val outputFile = new FakeFile("toss.out")
            val cmd = new ShellCommand("shcmd")
            cmd('command -> "cat", 'inputFile -> tempFile, 'outputFile -> outputFile)
            outputFile.stringForReading mustEqual "hello world!\n"
        }

        "map 'outputFile -> sake.util.File to the sink for output written from the subprocess" in {
            doInputTextOutputFile
        }
    }

}
