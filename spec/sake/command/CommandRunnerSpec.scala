package sake.command

import org.specs._
import sake.util._
import sake.environment._
import java.io.{PrintStream, ByteArrayOutputStream, BufferedReader}

object CommandRunnerSpec extends Specification {
    "A CommandRunner" should {
        "require a non-empty command name" in {
            new CommandRunner("") must throwA[BuildError]
        }

        "accept a list of command arguments" in {
            val args = List("-e", "println(List(1,2.0,\"three\"))")
            val runner = new CommandRunner("scala", args)
            runner.arguments mustEqual args
        }

        "accept an option map of environment/configuration values" in {
            val runner = new CommandRunner("scala", Nil, None)
            runner.arguments mustEqual Nil
            val environment = Some(Map[Any, Any]('directory -> "lib"))
            val runner2 = new CommandRunner("scala", Nil, environment)
            runner2.environment mustEqual environment

        }

        "accept an environment option 'directory to specify the directory to use as the working directory" in {
            runSuccessfulTestCommand
        }

        def doInputTextOutputFile(outputFile: File) {
            val environment = Some(Map[Any, Any]('inputText -> """Hello
                World""", 'outputFile -> outputFile))
            val runner = new CommandRunner("cat", Nil, environment)
            runner.environment mustEqual environment
            runner.run()
            testOutput(outputFile)
        }

        def testOutput(outputFile: File) = {
            val actual = outputFile match {
                case ff:FakeFile => ff.stringForReading
                case _ => readFileContents(outputFile)
            }
            """Hello[\r\n]+\s*World""".r findFirstIn actual match {
                case None => fail("actual: "+actual)
                case Some(_) =>
            }
        }

        def readFileContents(file: File): String = {
            val sb = new StringBuffer()
            val reader = new BufferedReader(file.reader)
            while(true) {
                reader.readLine() match {
                    case null => return sb.toString()
                    case line => sb.append(line+Environment.default.lineSeparator)
                }
            }
            sb.toString()
        }

        "accept an environment option 'inputText to specify a string of input for the subprocess" in {
            doInputTextOutputFile(new FakeFile("toss.out"))
        }

        def makeTempFileWithContent(tempFile: File) = {
            val writer = tempFile.writer
            writer.write("""Hello
                World""")
            writer.flush()
            writer.close()
            tempFile
        }

        "accept an environment option 'inputFile to specify a file for input to the subprocess" in {
            val tempFile = makeTempFileWithContent(new FakeFile("tossInput.txt"))
            val outputFile = new FakeFile("toss.out")
            val environment = Some(Map[Any, Any]('inputFile -> tempFile, 'outputFile -> outputFile))
            val runner = new CommandRunner("cat", Nil, environment)
            runner.environment mustEqual environment
            runner.run()
            testOutput(outputFile)
        }

        "accept an environment option 'inputFile to specify the name of a file for input to the subprocess" in {
            val inFile = File("tossInput.txt")
            val tempFile = makeTempFileWithContent(inFile)
            val outputFile = new FakeFile("toss.out")
            val environment = Some(Map[Any, Any]('inputFile -> tempFile, 'outputFile -> outputFile))
            val runner = new CommandRunner("cat", Nil, environment)
            runner.environment mustEqual environment
            runner.run()
            testOutput(outputFile)
            inFile.delete mustEqual true
        }

        "accept an environment option 'outputFile to specify where to write the output from the subprocess, as a string" in {
            doInputTextOutputFile(new FakeFile("toss.out"))
        }

        "accept an environment option 'outputFile to specify the name of a file to write the output from the subprocess, as a string" in {
            val outFile = File("toss.out")
            doInputTextOutputFile(outFile)
            outFile.delete mustEqual true
        }

        "treat any other environment option as an environment variable to set" in {
            val environment = Some(Map[Any, Any]('directory -> "lib", "FOO_BAR" -> "foobar"))
            val runner = new CommandRunner("pwd", Nil, environment)
            runner.environment mustEqual environment
            runner.processBuilder.environment.get("FOO_BAR") mustEqual "foobar"
        }

        "run a successful external program and process its output" in {
            runSuccessfulTestCommand
        }

        "run an unsuccessful external program and fail" in {
            runFailedTestCommand
        }

    }

    protected def runSuccessfulTestCommand = {
        val outputFile = new FakeFile("toss.out")
        val environment = Some(Map[Any, Any]('directory -> "lib", 'outputFile -> outputFile))
        val runner = new CommandRunner("pwd", Nil, environment)
        runner.environment mustEqual environment
        runner.run()
        outputFile.writer.toString must be matching ("""sake/lib$""")
    }

    protected def runFailedTestCommand = {
        val outputFile = new FakeFile("toss.out")
        val runner = new CommandRunner("ls nonexistentfile")
        runner.run().success must beFalse
    }
}

