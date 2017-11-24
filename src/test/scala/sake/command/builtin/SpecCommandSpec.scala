package sake.command.builtin

import org.scalatest._
import org.scalatest.Matchers._
import sake.util._
import sake.command._
import sake.context.Environment._

object SpecCommandSpec extends FreeSpec {
    def makeTestSpecCommand(expectedPath: String, expectedPattern: String) = {
        new SpecCommand() {
            override def action(options: Map[Symbol, Any]):Result = {
                val path    = options.getOrElse('path,    "spec").toString()
                val pattern = options.getOrElse('pattern, ".*").toString()
                path    shouldEqual expectedPath
                pattern shouldEqual expectedPattern
                Passed()
            }
            // Don't remove 'path and 'pattern, so tests in "action" can work.
            override def removePathAndPattern(options: Map[Symbol,Any]) = options
        }
    }

    "SpecCommand" should {
        "default to 'spec' for the path" in {
            val sc = makeTestSpecCommand("spec", ".*")
            sc()
        }

        "default to '.*' for the pattern" in {
            val sc = makeTestSpecCommand("spec", ".*")
            sc()
        }
    }

    "Invoking a SpecCommand object" should {
        "override the path if 'path is specified" in {
            val sc = makeTestSpecCommand("foo", ".*")
            sc('path -> "foo")
        }

        "override the path if 'pattern is specified" in {
            val sc = makeTestSpecCommand("spec", "Spec.*")
            sc('pattern -> "Spec.*")
        }
    }

    // TODO figure out why this fails, even though running the same
    // code at the Scala prompt (after importing ...) works fine.
    // "Invoking a failing spec" should {
    //     "throw a BuildError" in {
    //       try {
    //         new SpecCommand() (
    //             'classpath -> environment.classpath,
    //             'path -> "./spec/**/*.scala",
    //             'pattern -> "FailingSpek"
    //         ) //must throwA[BuildError]
    //       } catch {
    //         case th => th.isInstanceOf[BuildError] must beTrue
    //       }
    //       fail("Should have thrown a BuildError")
    //     }
    // }

    "Invoking a passing spec" should {
        "return Passed" in {
            new SpecCommand() (
                'path -> "./spec/**/*.scala",
                'pattern -> "PassingSpek"
            ).passed must beTrue
        }
    }
}

