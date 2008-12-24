package sake.command

import org.specs._
import sake.util._
import java.io.{PrintStream, ByteArrayOutputStream}

object CommandRunnerSpec extends Specification { 
    "A CommandRunner" should {
        "require a non-empty command name" in {
            new CommandRunner("") must throwA[BuildError]
        }

        "accept a list of command argumets" in {
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
            runLsJarsCommand
        }

        "accept an environment option 'input to specify the input to the subprocess, as a string" in {
            val environment = Some(Map[Any, Any]('input -> """Hello
                World"""))
            val runner = new CommandRunner("cat", Nil, environment)
            runner.environment mustEqual environment
            var byteStream  = new ByteArrayOutputStream()
            var newStream   = new PrintStream(byteStream)
            runner.outputPrintStream = newStream
            runner.run()
            val result = byteStream.toString()
            """cat:\s*Hello[\r\n]+cat:\s*World""".r findFirstIn result match {
                case None => fail(result)
                case Some(_) =>
            }
        }

        "treat any other environment option as an environment variable to set" in {
            val environment = Some(Map[Any, Any]('directory -> "lib", "FOO_BAR" -> "foobar"))
            val runner = new CommandRunner("pwd", Nil, environment)
            runner.environment mustEqual environment
            runner.processBuilder.environment.get("FOO_BAR") mustEqual "foobar"
        }

        "run an external program and process its output" in {
            runLsJarsCommand
        }
        
    }
    
    protected def runLsJarsCommand = {
        val environment = Some(Map[Any, Any]('directory -> "lib"))
        val runner = new CommandRunner("pwd", Nil, environment)
        runner.environment mustEqual environment
        var byteStream  = new ByteArrayOutputStream()
        var newStream   = new PrintStream(byteStream)
        runner.outputPrintStream = newStream
        runner.run()
        val result = byteStream.toString()
        """sake/lib$""".r findFirstIn result match {
            case None => fail(result)
            case Some(_) =>
        }
    }    
}

