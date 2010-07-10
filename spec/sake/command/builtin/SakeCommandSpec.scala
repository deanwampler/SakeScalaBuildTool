package sake.command.builtin

import sake.command._
import org.specs._ 

object SakeCommandSpec extends Specification {
    def makeTestSakeCommand(expectedFile: String, expectedTargets: String) = {
        new SakeCommand() {
            override def action(options: Map[Symbol, Any]):Result = {
                val command = options.getOrElse('command, "").toString()
                val input   = options.getOrElse('inputText, "").toString()
                command mustEqual "scala"
                (":load "+expectedFile+"\\s*[\\n\\r]+\\s*build\\(\""+expectedTargets+"\"\\)").r findFirstIn input match {
                    case None => fail(input)
                    case Some(_) => 
                }
                new Passed()
            } 
        }
    }
    
    "SakeCommand" should {
        "default to 'sake.scala' for the sake file" in {
            val sc = makeTestSakeCommand("sake.scala", "all")
            sc()
        }

        "default to 'all' for the targets" in {
            val sc = makeTestSakeCommand("sake.scala", "all")
            sc()
        }
    }
    
    "Invoking a SakeCommand object" should {
        "override the sake file if 'f is specified" in {
            val sc = makeTestSakeCommand("sake2.scala", "all")
            sc('f -> "sake2.scala")
        }

        "override the sake file if 'file is specified" in {
            val sc = makeTestSakeCommand("sake2.scala", "all")
            sc('file -> "sake2.scala")
        }

        "override the targets if 'targets are specified" in {
            val sc = makeTestSakeCommand("sake.scala", "foo bar")
            sc('targets -> "foo bar")
        }

        "override the targets if 'targets are specified and convert symbols to strings for JVM invocation" in {
            val sc = makeTestSakeCommand("sake.scala", "foo bar")
            sc('targets -> List("foo", 'bar))
        }
    }
    
}

