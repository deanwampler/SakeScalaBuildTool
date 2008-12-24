package sake.command.builtin

import org.specs._ 

object SpecCommandSpec extends Specification {
    def makeTestSpecCommand(expectedPath: String, expectedPattern: String, expectedReportFlag: Boolean) = {
        new SpecCommand() {
            override def action(options: Map[Symbol, Any]):Result = {
                val path    = options.getOrElse('path,    "spec").toString()
                val pattern = options.getOrElse('pattern, ".*").toString()
                val report  = options.getOrElse('report,  true)
                path    mustEqual expectedPath
                pattern mustEqual expectedPattern
                report  mustEqual expectedReportFlag
                new Passed()
            } 
        }
    }
    
    "SpecCommand" should {
        "default to 'spec' for the path" in {
            val sc = makeTestSpecCommand("spec", ".*", true)
            sc()
        }

        "default to '.*' for the pattern" in {
            val sc = makeTestSpecCommand("spec", ".*", true)
            sc()
        }

        "default to 'true' for the report output flag" in {
            val sc = makeTestSpecCommand("spec", ".*", true)
            sc()
        }
    }
    
    "Invoking a SpecCommand object" should {
        "override the path if 'path is specified" in {
            val sc = makeTestSpecCommand("foo", ".*", true)
            sc('path -> "foo")
        }

        "override the path if 'pattern is specified" in {
            val sc = makeTestSpecCommand("spec", "Spec.*", true)
            sc('pattern -> "Spec.*")
        }

        "override the report output flag if 'report is specified" in {
            val sc = makeTestSpecCommand("spec", ".*", false)
            sc('report -> false)
        }
    }
    
}

