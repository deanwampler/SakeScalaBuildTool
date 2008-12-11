package sake.command

import org.specs._
import java.io.{PrintStream, ByteArrayOutputStream}

object CommandRunnerSpec extends Specification { 
    "A CommandRunner" should {
        "run an external program and process its output" in {
            val runner = new CommandRunner("scala", List("-e", "println(List(1,2.0,\"three\"))"))
            var byteStream  = new ByteArrayOutputStream()
            var newStream   = new PrintStream(byteStream)
            runner.outputPrintStream = newStream
            runner.run()
            byteStream.toString() must be_==("scala: List(1, 2.0, three)\n")
        }
    }
}

