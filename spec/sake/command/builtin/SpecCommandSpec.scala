package sake.command.builtin

import org.specs._ 

object SpecCommandSpec extends Specification {
    def makeTestSpecCommand(expectedPath: String, expectedPattern: String) = {
        new SpecCommand() {
            override def makeSpecsFileRunner(path: String, pattern: String) = {
                path    must be_==(expectedPath)
                pattern must be_==(expectedPattern)
                super.makeSpecsFileRunner(path, pattern)
            } 
            override def runSpec(spec: org.specs.Specification) = {}
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
    
}

